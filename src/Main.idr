module Main

import Idris.Driver

import Pretty
import StringMap
import SortedSet

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.Scheme.Common

import Core.Hash
import Core.Context
import Core.Directory
import Core.Name
import Core.Name.Namespace
import Core.Options
import Core.TT
import Libraries.Utils.Hex
import Libraries.Utils.Path

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect
import Libraries.Data.NameMap

import System
import System.Directory
import System.File
import System.Info

%default covering

-- this will break if the same scheme foreign function is used to implement
-- an operation on, say, different numeric types, as pointed out by @karroffel
--
-- let's go with this for now, though
--
emulatedForeigns : StringMap String
emulatedForeigns = StringMap.fromList
  -- general
  [ ("scheme:string-concat", "Rts.Bytes.concat")
  , ("scheme:blodwen-args",  "Rts.System.get_args")
  , ("scheme:string-pack",   "Rts.String.pack")
  , ("scheme:string-unpack", "Rts.String.unpack")
  , ("scheme:blodwen-string-iterator-new",  "Rts.String.Iterator.new_")
  , ("scheme:blodwen-string-iterator-next", "Rts.String.Iterator.next")

  -- clock
  , ("scheme:blodwen-clock-time-gccpu", "Rts.System.clocktime_gc_cpu")
  , ("scheme:blodwen-clock-time-gcreal", "Rts.System.clocktime_gc_real")
  , ("scheme:blodwen-clock-time-monotonic", "Rts.System.clocktime_monotonic")
  , ("scheme:blodwen-clock-time-process", "Rts.System.clocktime_process")
  , ("scheme:blodwen-clock-time-thread", "Rts.System.clocktime_thread")
  , ("scheme:blodwen-clock-time-utc", "Rts.System.clocktime_utc")
  , ("scheme:blodwen-clock-second", "Rts.System.os_clock_second")
  , ("scheme:blodwen-clock-nanosecond", "Rts.System.os_clock_nanosecond")
  , ("scheme:blodwen-is-time?", "Rts.System.os_clock_valid")

  -- chez refuses to use foreign C functions for buffers
  -- probably as a reminder that the backend-specific versions will be faster
  , ("scheme:blodwen-new-buffer", "Rts.C.Lib_libidris2_support.idris2_newBuffer")
  , ("scheme:blodwen-buffer-size", "Rts.C.Lib_libidris2_support.idris2_getBufferSize")
  , ("scheme:blodwen-buffer-copydata", "Rts.C.Lib_libidris2_support.idris2_copyBuffer")
  , ("scheme:blodwen-buffer-getbyte", "Rts.C.Lib_libidris2_support.idris2_getBufferByte")
  , ("scheme:blodwen-buffer-getint", "Rts.C.Lib_libidris2_support.idris2_getBufferInt")
  , ("scheme:blodwen-buffer-getdouble", "Rts.C.Lib_libidris2_support.idris2_getBufferDouble")
  , ("scheme:blodwen-buffer-getstring", "Rts.C.Lib_libidris2_support.idris2_getBufferString")
  , ("scheme:blodwen-buffer-setbyte", "Rts.C.Lib_libidris2_support.idris2_setBufferByte")
  , ("scheme:blodwen-buffer-setint", "Rts.C.Lib_libidris2_support.idris2_setBufferInt")
  , ("scheme:blodwen-buffer-setdouble", "Rts.C.Lib_libidris2_support.idris2_setBufferDouble")
  , ("scheme:blodwen-buffer-setstring", "Rts.C.Lib_libidris2_support.idris2_setBufferString")
  ]

heXX : Int -> String
heXX x = hd (x `div` 16) ++ hd (x `mod` 16)
  where
    hd : Int -> String
    hd 10 = "A"
    hd 11 = "B"
    hd 12 = "C"
    hd 13 = "D"
    hd 14 = "E"
    hd 15 = "F"
    hd i = show i

showChar : Char -> String -> String
showChar '\\' = ("\\\\" ++)
showChar '"' = ("\\\"" ++)
showChar '\n' = ("\\n" ++)
showChar c
   = if c < chr 32
        then (("\\x" ++ heXX (cast c) ++ "") ++)
        else strCons c

showString : List Char -> String -> String
showString [] = id
showString (c::cs) = showChar c . showString cs

mlfString : String -> Doc
mlfString cs = text $ strCons '"' (showString (unpack cs) "\"")

sexp : List Doc -> Doc
sexp = parens . hsep

mlfGlobal : String -> Doc
mlfGlobal mlName = parens $
  text "global"
    <++> hsep
      [ text ("$" ++ n)
      | n <- toList $ split (== '.') mlName
      ]

mlfApply : Doc -> List Doc -> Doc
mlfApply f [] = f
mlfApply f args = parens $
  text "apply" <++> f
  $$ indentBlock args

mlfLibCall : String -> List Doc -> Doc
mlfLibCall fn args = mlfApply (mlfGlobal fn) args

mlfError : String -> Doc
mlfError msg = mlfLibCall "Stdlib.failwith" [mlfString msg]

mlfDebug : Show a => a -> Doc
mlfDebug = mlfError . show

sanitise : String -> String
sanitise = pack . concatMap sanitise' . unpack
  where
    san : Char -> Bool
    san c =
        ('A' <= c && c <= 'Z')
        || ('a' <= c && c <= 'z')
        || ('0' <= c && c <= '9')

    sanitise' : Char -> List Char
    sanitise' c =
      if san c
        then [c]
        else '_' :: unpack (show $ ord c) ++ ['_']

mlfName : Name -> Doc
mlfName (MN n i) = text (sanitise n) <+> show i
mlfName n = text . sanitise . schName $ n

-- ML identifiers can't start with capital letters
mlfGlobalName : Name -> Doc
mlfGlobalName n = text "idr_" <+> mlfName n

mlfLocalVar : Name -> Doc
mlfLocalVar n = text "$" <+> mlfName n

mlfGlobalVar : Name -> Doc
mlfGlobalVar n = text "$" <+> mlfGlobalName n

-- returns MLF module name
mlfNS : Name -> String
mlfNS (NS ns n) = "Mod_" ++ concat (intersperse "_" $ reverse $ unsafeUnfoldNamespace ns)
mlfNS n = "Misc"

record ModuleName where
  constructor MkMN
  string : String

Eq ModuleName where
  MkMN x == MkMN y = x == y

mlfGlobalNS : StringMap ModuleName -> ModuleName -> Name -> Doc
mlfGlobalNS nsMap curModuleName n =
  let mns = mlfNS n
    in case StringMap.lookup mns nsMap of
      Nothing => mlfError $ "mlfGlobalNS: impossible: could not find " ++ show mns
      Just targetModName =>
        if targetModName == curModuleName
          then mlfGlobalVar n  -- within-module reference
          else sexp [text "global", text ("$" ++ targetModName.string), mlfGlobalVar n]

mlfLet : Name -> Doc -> Doc -> Doc
mlfLet n val rhs = parens $
  text "let"
  $$ indentBlock
    [ sexp [mlfLocalVar n, val]
    , rhs
    ]

mlfLazy : Doc -> Doc
mlfLazy doc = sexp [text "lazy", doc]

mlfLam : List Name -> Doc -> Doc
mlfLam [] rhs = mlfLazy rhs
mlfLam args rhs =
  parens $
    text "lambda" <++> sexp (map mlfLocalVar args)
    $$ indent rhs

mlfForce : Doc -> Doc
mlfForce doc = sexp [text "force", doc]

mlfBlock : Maybe Int -> List Doc -> Doc
mlfBlock Nothing args = mlfError "no constructor tag (1)"
mlfBlock (Just tag) args = parens $
  text "block" <++> sexp [text "tag", show tag]
  $$ indentBlock args

mlfCmp : String -> String -> String -> List Doc -> Doc
mlfCmp conv cmp zero args = sexp [text cmp, mlfLibCall conv args, text zero]

mlfOp : PrimFn arr -> Vect arr Doc -> Doc
mlfOp (Add IntType) [x,y] = sexp [text "+.int", x,y]
mlfOp (Sub IntType) [x,y] = sexp [text "-.int", x,y]
mlfOp (Mul IntType) [x,y] = sexp [text "*.int", x,y]
mlfOp (Div IntType) [x,y] = sexp [text "/.int", x,y]
mlfOp (Mod IntType) [x,y] = sexp [text "%.int", x,y]
mlfOp (Neg IntType) [x]   = sexp [text "neg.int", x]
mlfOp (ShiftL IntType) [x,y] = sexp [text "<<.int", x, y]
mlfOp (ShiftR IntType) [x,y] = sexp [text ">>.int", x, y]
mlfOp (BAnd IntType)   [x,y] = sexp [text "&.int", x, y]
mlfOp (BOr IntType)    [x,y] = sexp [text "|.int", x, y]
mlfOp (BXOr IntType)   [x,y] = sexp [text "^.int", x, y]

mlfOp (LT IntType) [x,y] = sexp [text "<.int", x,y]
mlfOp (LTE IntType) [x,y] = sexp [text "<=.int", x,y]
mlfOp (EQ IntType) [x,y] = sexp [text "==.int", x,y]
mlfOp (GTE IntType) [x,y] = sexp [text ">=.int", x,y]
mlfOp (GT IntType) [x,y] = sexp [text ">.int", x,y]

mlfOp (LT CharType) [x,y] = sexp [text "<.int", x,y]
mlfOp (LTE CharType) [x,y] = sexp [text "<=.int", x,y]
mlfOp (EQ CharType) [x,y] = sexp [text "==.int", x,y]
mlfOp (GTE CharType) [x,y] = sexp [text ">=.int", x,y]
mlfOp (GT CharType) [x,y] = sexp [text ">.int", x,y]

mlfOp (Add IntegerType) [x,y] = sexp [text "+.ibig", x,y]
mlfOp (Sub IntegerType) [x,y] = sexp [text "-.ibig", x,y]
mlfOp (Mul IntegerType) [x,y] = sexp [text "*.ibig", x,y]
mlfOp (Div IntegerType) [x,y] = sexp [text "/.ibig", x,y]
mlfOp (Mod IntegerType) [x,y] = sexp [text "%.ibig", x,y]
mlfOp (Neg IntegerType) [x]   = sexp [text "neg.ibig", x]
mlfOp (ShiftL IntegerType) [x,y] = sexp [text "<<.ibig", x, y]
mlfOp (ShiftR IntegerType) [x,y] = sexp [text ">>.ibig", x, y]
mlfOp (BAnd IntegerType)   [x,y] = sexp [text "&.ibig", x, y]
mlfOp (BOr IntegerType)    [x,y] = sexp [text "|.ibig", x, y]
mlfOp (BXOr IntegerType)   [x,y] = sexp [text "^.ibig", x, y]

mlfOp (LT IntegerType) [x,y] = sexp [text "<.ibig", x,y]
mlfOp (LTE IntegerType) [x,y] = sexp [text "<=.ibig", x,y]
mlfOp (EQ IntegerType) [x,y] = sexp [text "==.ibig", x,y]
mlfOp (GTE IntegerType) [x,y] = sexp [text ">=.ibig", x,y]
mlfOp (GT IntegerType) [x,y] = sexp [text ">.ibig", x,y]

mlfOp (Add DoubleType) [x,y] = sexp [text "+.f64", x,y]
mlfOp (Sub DoubleType) [x,y] = sexp [text "-.f64", x,y]
mlfOp (Mul DoubleType) [x,y] = sexp [text "*.f64", x,y]
mlfOp (Div DoubleType) [x,y] = sexp [text "/.f64", x,y]
mlfOp (Mod DoubleType) [x,y] = sexp [text "%.f64", x,y]
mlfOp (Neg DoubleType) [x]   = sexp [text "neg.f64", x]

mlfOp (LT DoubleType) [x,y] = sexp [text "<.f64", x,y]
mlfOp (LTE DoubleType) [x,y] = sexp [text "<=.f64", x,y]
mlfOp (EQ DoubleType) [x,y] = sexp [text "==.f64", x,y]
mlfOp (GTE DoubleType) [x,y] = sexp [text ">=.f64", x,y]
mlfOp (GT DoubleType) [x,y] = sexp [text ">.f64", x,y]

mlfOp DoubleExp [x] = mlfLibCall "Float.exp" [x]
mlfOp DoubleLog [x] = mlfLibCall "Float.log" [x]  -- should this be Float.log10?
mlfOp DoubleSin [x] = mlfLibCall "Float.sin" [x]
mlfOp DoubleCos [x] = mlfLibCall "Float.cos" [x]
mlfOp DoubleTan [x] = mlfLibCall "Float.tan" [x]
mlfOp DoubleASin [x] = mlfLibCall "Float.asin" [x]
mlfOp DoubleACos [x] = mlfLibCall "Float.acos" [x]
mlfOp DoubleATan [x] = mlfLibCall "Float.atan" [x]
mlfOp DoubleSqrt [x] = mlfLibCall "Float.sqrt" [x]
mlfOp DoubleFloor [x] = mlfLibCall "Float.floor" [x]
mlfOp DoubleCeiling [x] = mlfLibCall "Float.ceil" [x]

mlfOp (Cast IntegerType DoubleType) [x] = sexp [text "convert.ibig.f64", x]
mlfOp (Cast DoubleType IntegerType) [x] = sexp [text "convert.f64.ibig", x]
mlfOp (Cast IntType DoubleType) [x] = sexp [text "convert.int.f64", x]
mlfOp (Cast DoubleType IntType) [x] = sexp [text "convert.f64.int", x]
mlfOp (Cast IntegerType IntType) [x] = sexp [text "convert.ibig.int", x]
mlfOp (Cast IntType IntegerType) [x] = sexp [text "convert.int.ibig", x]
mlfOp (Cast IntegerType CharType) [x] = sexp [text "convert.ibig.int", x]
mlfOp (Cast CharType IntegerType) [x] = sexp [text "convert.int.ibig", x]
mlfOp (Cast CharType IntType) [x] = x
mlfOp (Cast IntType CharType) [x] = x
mlfOp (Cast IntegerType StringType) [x] = mlfLibCall "Z.to_string" [x]
mlfOp (Cast IntType StringType) [x] = mlfLibCall "Stdlib.string_of_int" [x]
mlfOp (Cast StringType IntegerType) [x] = mlfLibCall "Z.of_string" [x]
mlfOp (Cast StringType IntType) [x] = mlfLibCall "Stdlib.int_of_string" [x]
mlfOp (Cast CharType StringType) [x] = mlfLibCall "Rts.String.of_char" [x]
mlfOp (Cast StringType CharType) [x] = mlfLibCall "Rts.String.head" [x]
mlfOp (Cast StringType DoubleType) [x] = mlfLibCall "Float.of_string" [x]
mlfOp (Cast DoubleType StringType) [x] = mlfLibCall "Float.to_string" [x]

mlfOp StrLength [x] = mlfLibCall "Rts.String.length" [x]
mlfOp StrHead [x] = mlfLibCall "Rts.String.head" [x]
mlfOp StrTail [x] = mlfLibCall "Rts.String.tail" [x]
mlfOp StrIndex [x, i] = mlfLibCall "Rts.String.get" [x, i]
mlfOp StrCons [x, xs] = mlfLibCall "Rts.String.cons" [x, xs]
mlfOp StrReverse [x] = mlfLibCall "Rts.String.reverse" [x]
mlfOp StrSubstr [off, len, s] = mlfLibCall "Rts.String.substring" [off, len, s]
mlfOp StrAppend [x,y] = mlfLibCall "Bytes.cat" [x, y]

mlfOp (LT StringType) [x,y] = mlfCmp "String.compare" "<.int" "0" [x,y]
mlfOp (LTE StringType) [x,y] = mlfCmp "String.compare" "<=.int" "0" [x,y]
mlfOp (EQ StringType) [x,y] = mlfCmp "String.compare" "==.int" "0" [x,y]
mlfOp (GTE StringType) [x,y] = mlfCmp "String.compare" ">=.int" "0" [x,y]
mlfOp (GT StringType) [x,y] = mlfCmp "String.compare" ">.int" "0" [x,y]

mlfOp Crash [_, msg] = mlfLibCall "Stdlib.failwith" [msg]
mlfOp BelieveMe [_, _, x] = x

mlfOp op args = mlfError $ "unimplemented primop: " ++ show op

private
un : String -> Name
un = UN . Basic

mlfExtPrim : Name -> Doc
mlfExtPrim (NS _ (UN (Basic "prim__newArray"))) =
  mlfLam [un "_ty", un "n", un "x", un "_world"] $
    sexp [text "makevec", mlfLocalVar (un "n"), mlfLocalVar (un "x")]
mlfExtPrim (NS _ (UN (Basic "prim__arrayGet"))) =
  mlfLam [un "_ty", un "arr", un "i", un "_world"] $
    sexp [text "load", mlfLocalVar (un "arr"), mlfLocalVar (un "i")]
mlfExtPrim (NS _ (UN (Basic "prim__arraySet"))) =
  mlfLam [un "_ty", un "arr", un "i", un "x", un "_world"] $
    sexp [text "store", mlfLocalVar (un "arr"), mlfLocalVar (un "i"), mlfLocalVar (un "x")]
mlfExtPrim (NS _ (UN (Basic "prim__newIORef"))) =
  mlfLam [un "_ty", un "x", un "_world"] $
    sexp [text "makevec", show 1, mlfLocalVar (un "x")]
mlfExtPrim (NS _ (UN (Basic "prim__readIORef"))) =
  mlfLam [un "_ty", un "ref", un "_world"] $
    sexp [text "load", mlfLocalVar (un "ref"), show 0]
mlfExtPrim (NS _ (UN (Basic "prim__writeIORef"))) =
  mlfLam [un "_ty", un "ref", un "x", un "_world"] $
    sexp [text "store", mlfLocalVar (un "ref"), show 0, mlfLocalVar (un "x")]
mlfExtPrim (NS _ (UN (Basic "prim__schemeCall"))) =
  mlfLam [un "_rTy", un "fn", un "_args", un "_world"] $
    mlfLibCall "Stdlib.failwith" [mlfLocalVar (un "fn")]
mlfExtPrim (NS _ (UN (Basic "prim__codegen"))) = mlfString "malfunction"
mlfExtPrim (NS _ (UN (Basic "prim__os"))) = mlfGlobal "Rts.System.os_name"
mlfExtPrim n = mlfError $ "unimplemented external primitive: " ++ show n

mlfConstant : Constant -> Doc
mlfConstant (I x) = show x
mlfConstant (BI x) = show x <+> text ".ibig"
mlfConstant (Str s) = mlfString s
mlfConstant (Ch x) = show (ord x)
mlfConstant (Db x) =
  case filter (== '.') (unpack $ Prelude.show x) of
    [] => Pretty.show x <+> text ".0"
    _  => Pretty.show x

mlfConstant (I8 x) = mlfError $ "I8: " ++ show x
mlfConstant (I16 x) = mlfError $ "I16: " ++ show x
mlfConstant (I32 x) = mlfError $ "I32: " ++ show x
mlfConstant (I64 x) = mlfError $ "I64: " ++ show x

mlfConstant (B8 x) = mlfError $ "B8: " ++ show x
mlfConstant (B16 x) = mlfError $ "B16: " ++ show x
mlfConstant (B32 x) = mlfError $ "B32: " ++ show x
mlfConstant (B64 x) = mlfError $ "B64: " ++ show x

mlfConstant WorldVal = show 0

mlfConstant IntType = show 0
mlfConstant IntegerType = show 1
mlfConstant StringType = show 2
mlfConstant CharType = show 3
mlfConstant DoubleType = show 4
mlfConstant WorldType = show 5
mlfConstant Bits8Type = show 6
mlfConstant Bits16Type = show 7
mlfConstant Bits32Type = show 8
mlfConstant Bits64Type = show 9
mlfConstant Int8Type = show 10
mlfConstant Int16Type = show 11
mlfConstant Int32Type = show 12
mlfConstant Int64Type = show 13

mlfConstPat : Constant -> Maybe Doc
-- malfunction cannot switch on these
mlfConstPat (BI x) = Nothing
mlfConstPat (Str s) = Nothing
mlfConstPat (Db x) = Nothing
mlfConstPat c = Just $ mlfConstant c

mlfConstEqCheck : Doc -> Constant -> Doc
-- these have special comparison ops
mlfConstEqCheck x (BI y) = sexp [text "==.ibig", x, mlfConstant (BI y)]
mlfConstEqCheck x (Db y) = sexp [text "==.f64", x, mlfConstant (Db y)]
mlfConstEqCheck x (Str y) = mlfLibCall "String.equal" [x, mlfConstant (Str y)]

-- everything else is represented as ints
mlfConstEqCheck x y = sexp [text "==.int", x, mlfConstant y]

mlfConDflt : Doc -> Doc
mlfConDflt rhs = sexp [sexp [text "tag", text "_"], text "_", rhs]

mlfSwitch : Doc -> List Doc -> Maybe Doc -> Doc
mlfSwitch scrut [] Nothing =
  mlfError $ "case with no RHS"
mlfSwitch scrut [] (Just dflt) = dflt
mlfSwitch scrut alts (Just dflt) = parens $
  text "switch" <++> scrut
  $$ indent (vcat alts $$ dflt)

mlfSwitch scrut alts Nothing = parens $
  text "switch" <++> scrut
  $$ indent (if debug then (vcat alts $$ catchall) else vcat alts)
 where
  debug : Bool
  debug = True

  catchall : Doc
  catchall =
    mlfConDflt $
      mlfLet (un "_") (mlfLibCall "Rts.Debug.inspect" [show 0, scrut])$
        mlfError "unmatched pattern! (block tree dump above)"

mlfConstDflt : Doc -> Doc
mlfConstDflt rhs = sexp [text "_", rhs]

mlfField : Name -> Int -> Doc
mlfField n i = sexp [text "field", show i, mlfLocalVar n]

number : Int -> List a -> List (Int, a)
number i [] = []
number i (x :: xs) = (i,x) :: number (i+1) xs

bindFieldProjs : Name -> List Name -> Doc -> Doc
bindFieldProjs scrutN [] rhs = rhs
bindFieldProjs scrutN ns rhs = parens $
  text "let"
  $$ indent (
    vcat [sexp [mlfLocalVar n, mlfField scrutN i] | (i, n) <- number 0 ns]
    $$ rhs
  )

ccLibFun : List String -> Maybe String
ccLibFun [] = Nothing
ccLibFun (cc :: ccs) =
  case StringMap.lookup cc emulatedForeigns of
    Just result => Just result
    Nothing =>
      if substr 0 3 cc == "ML:"
        then Just (substr 3 (length cc) cc)
        else if substr 0 2 cc == "C:"
            then case split (== ',') (substr 2 (length cc) cc) of
              fn ::: [libn] => Just ("Rts.C.Lib_" ++ rmSpaces libn ++ "." ++ fn)
              _ => ccLibFun ccs  -- something strange -> skip
            else ccLibFun ccs  -- search further
  where
    rmSpaces : String -> String
    rmSpaces = pack . filter (/= ' ') . unpack

{-
unApp : NamedCExp -> List NamedCExp -> (NamedCExp, List NamedCExp)
unApp (NmApp fc f args) args' = unApp f (args ++ args')
unApp f args = (f, args)
-}

-- namespaces mentioned within
mutual
  nsTm : NamedCExp -> SortedSet String
  nsTm (NmLocal fc n) = SortedSet.empty
  nsTm (NmRef fc n) = SortedSet.singleton $ mlfNS n
  nsTm (NmLam fc n rhs) = nsTm rhs
  nsTm (NmLet fc n val rhs) = nsTm val <+> nsTm rhs
  nsTm (NmApp fc f args) = nsTm f <+> concatMap nsTm args
  nsTm (NmCon fc cn ci tag args) = concatMap nsTm args
  nsTm (NmForce fc lr rhs) = nsTm rhs
  nsTm (NmDelay fc lr rhs) = nsTm rhs
  nsTm (NmErased fc) = SortedSet.empty
  nsTm (NmPrimVal ft x) = SortedSet.empty
  nsTm (NmOp fc op args) = concatMap nsTm args
  nsTm (NmExtPrim fc n args) = concatMap nsTm args
  nsTm (NmConCase fc scrut alts mbDflt) =
    nsTm scrut <+> concatMap nsConAlt alts <+> concatMap nsTm mbDflt
  nsTm (NmConstCase fc scrut alts mbDflt) =
    nsTm scrut <+> concatMap nsConstAlt alts <+> concatMap nsTm mbDflt
  nsTm (NmCrash fc msg) = SortedSet.empty

  nsConAlt : NamedConAlt -> SortedSet String
  nsConAlt (MkNConAlt n ci tag args rhs) = nsTm rhs

  nsConstAlt : NamedConstAlt -> SortedSet String
  nsConstAlt (MkNConstAlt c rhs) = nsTm rhs

nsDef : NamedDef -> SortedSet String
nsDef (MkNmFun argNs rhs) = nsTm rhs
nsDef (MkNmCon tag arity nt) = SortedSet.empty
nsDef (MkNmForeign ccs fargs rty) = SortedSet.empty
nsDef (MkNmError rhs) = nsTm rhs

parameters (ldefs : SortedSet Name, nsMapping : StringMap ModuleName, curModuleName : ModuleName)
  mutual
    bindScrut : NamedCExp -> (Name -> Doc) -> Doc
    bindScrut (NmLocal _ n) rhs = rhs n
    bindScrut scrut rhs =
      let scrutN = MN "scrut" 0
        in mlfLet scrutN (mlfTm scrut) (rhs scrutN)

    mlfEqChain : Name -> Maybe Doc -> List NamedConstAlt -> Doc
    mlfEqChain scrutN Nothing [] = mlfError "impossible eq chain"
    mlfEqChain scrutN (Just dflt) [] = dflt
    mlfEqChain scrutN mbDflt (MkNConstAlt c rhs :: alts) = parens $
      text "if" <++> mlfConstEqCheck (mlfLocalVar scrutN) c
      $$ indent (
        mlfTm rhs
        $$ mlfEqChain scrutN mbDflt alts
      )

    mlfConAlt : Name -> NamedConAlt -> Doc
    mlfConAlt scrutN (MkNConAlt cn ci Nothing args rhs) =
      mlfError $ "no tag for mlfConAlt: " ++ show cn
    mlfConAlt scrutN (MkNConAlt cn ci (Just tag) [] rhs) =
      -- nullary constructors compile to ints in ocaml
      sexp [show tag, mlfTm rhs]
    mlfConAlt scrutN (MkNConAlt cn ci (Just tag) args rhs) = parens $
      sexp [text "tag", show tag]
      $$ indent (bindFieldProjs scrutN args $ mlfTm rhs)

    mlfConstAlt : NamedConstAlt -> Maybe Doc
    mlfConstAlt (MkNConstAlt c rhs) =
      case mlfConstPat c of
        Just pat => Just $ parens (pat <++> mlfTm rhs)
        Nothing => Nothing

    mlfTm : NamedCExp -> Doc
    mlfTm (NmLocal fc n) = mlfLocalVar n
    mlfTm (NmRef fc n) =
        if contains n ldefs
          then mlfForce (mlfGlobalNS nsMapping curModuleName n)
          else mlfGlobalNS nsMapping curModuleName n
    mlfTm (NmLam fc n rhs) = mlfLam [n] (mlfTm rhs)
    mlfTm (NmLet fc n val rhs) = mlfLet n (mlfTm val) (mlfTm rhs)
    mlfTm (NmApp fc f args) =
      mlfApply (mlfTm f) (map mlfTm args)
      {- probably unnecessary
      let (f', args') = unApp f args
        in mlfApply (mlfTm f') (map mlfTm args')
      -}
    mlfTm (NmCon fc cn ci Nothing []) = mlfString (show cn)  -- type constructor
    mlfTm (NmCon fc cn ci (Just tag) []) = show tag
    mlfTm (NmCon fc cn ci mbTag args) = mlfBlock mbTag (map mlfTm args)
    mlfTm (NmCrash fc msg) = mlfError msg
    mlfTm (NmForce fc lr rhs) = mlfForce (mlfTm rhs)
    mlfTm (NmDelay fc lr rhs) = mlfLazy (mlfTm rhs)
    mlfTm (NmErased fc) = mlfString "erased"
    mlfTm (NmPrimVal ft x) = mlfConstant x
    mlfTm (NmOp fc op args) = mlfOp op (map mlfTm args)
    mlfTm (NmExtPrim fc n args) = mlfApply (mlfExtPrim n) (map mlfTm args)
    mlfTm (NmConCase fc scrut alts mbDflt) =
      bindScrut scrut $ \scrutN =>
        mlfSwitch
          (mlfLocalVar scrutN)
          (map (mlfConAlt scrutN) alts)
          (mlfConDflt . mlfTm <$> mbDflt)
    mlfTm (NmConstCase fc scrut alts mbDflt) =
      case the (Maybe (List Doc)) (traverse mlfConstAlt alts) of
        -- all patterns can be expressed efficiently
        Just alts' =>
          mlfSwitch (mlfTm scrut) alts' (mlfConstDflt . mlfTm <$> mbDflt)

        -- we need to use a chain of if-equals tests
        Nothing =>
          bindScrut scrut $ \scrutN =>
            mlfEqChain scrutN (mlfTm <$> mbDflt) alts

  mlfBody : NamedDef -> Doc
  mlfBody (MkNmFun args rhs) =
    mlfLam args (mlfTm rhs)

  mlfBody (MkNmCon mbTag arity mbNewtype) =
      mlfLam args (mlfBlock mbTag $ map mlfLocalVar args)
    where
      args : List Name
      args = [un $ "arg" ++ show i | i <- [0..cast {to = Int} arity-1]]

  mlfBody (MkNmForeign ccs args cty) =
    mlfLam (map fst lamArgs) $
      case ccLibFun ccs of
        Just fn => mlfLibCall fn (map mlfLocalVar mlArgs)
        Nothing =>
          mlfError $ "unimplemented foreign: " ++ show (MkNmForeign ccs args cty)
    where
      mkArgs : Int -> List CFType -> List (Name, Bool)
      mkArgs i [] = []
      mkArgs i (CFWorld :: cs) = (MN "farg" i, False) :: mkArgs (i + 1) cs
      mkArgs i (c :: cs) = (MN "farg" i, True) :: mkArgs (i + 1) cs

      -- arguments of the Malfunction lambda
      lamArgs : List (Name, Bool)
      lamArgs = mkArgs 0 args

      -- arguments of the foreign ML function
      mlArgs : List Name
      mlArgs = map fst $ case lamArgs of
        -- if we have only one argument, we have to keep it
        -- even if it's %World
        -- to avoid turning the function into a non-function
        [_] => lamArgs

        -- otherwise, attempt to remove %World
        _ => filter snd lamArgs

  mlfBody (MkNmError err) =
    mlfTm err

  mlfDef : (Name, FC, NamedDef) -> Doc
  mlfDef (n, fc, body) =
    parens (mlfGlobalVar n $$ indent (mlfBody body))
    $$ text ""

lazyDefs : List (Name, FC, NamedDef) -> SortedSet Name
lazyDefs [] = empty
lazyDefs ((n,_,MkNmFun [] rhs) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmCon tag Z nt) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmForeign ccs [] x) :: defs) = insert n $ lazyDefs defs
lazyDefs ((n,_,MkNmError x) :: defs) = insert n $ lazyDefs defs
lazyDefs (_ :: defs) = lazyDefs defs

mlfRec : List Doc -> Doc
mlfRec defs = parens $
  text "rec"
  $$ indentBlock defs

splitByNS : List (Name, FC, NamedDef) -> List (String, List (Name, FC, NamedDef))
splitByNS = StringMap.toList . foldl addOne StringMap.empty
  where
    addOne
      : StringMap (List (Name, FC, NamedDef))
      -> (Name, FC, NamedDef)
      -> StringMap (List (Name, FC, NamedDef))
    addOne nss def@(n, fc, nd) =
      StringMap.mergeWith
        (++)
        (StringMap.singleton (mlfNS n) [def])
        nss

-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm#The_algorithm_in_pseudocode
record TarjanVertex where
  constructor TV
  index : Int
  lowlink : Int
  inStack : Bool

record TarjanState where
  constructor TS
  vertices : StringMap TarjanVertex
  stack : List String
  nextIndex : Int
  components : List (List String)
  impossibleHappened : Bool

tarjan : StringMap (SortedSet String) -> List (List String)
tarjan deps = loop initialState (StringMap.keys deps)
  where
    initialState : TarjanState
    initialState =
      TS
        StringMap.empty
        []
        0
        []
        False

    strongConnect : TarjanState -> String -> TarjanState
    strongConnect ts v =
        let ts'' = case StringMap.lookup v deps of
              Nothing => ts'  -- no edges
              Just edgeSet => loop ts' (SortedSet.toList edgeSet)
          in case StringMap.lookup v ts''.vertices of
              Nothing => record { impossibleHappened = True } ts''
              Just vtv =>
                if vtv.index == vtv.lowlink
                  then createComponent ts'' v []
                  else ts''
      where
        createComponent : TarjanState -> String -> List String -> TarjanState
        createComponent ts v acc =
          case ts.stack of
            [] => record { impossibleHappened = True } ts
            w :: ws =>
              let ts' = record {
                      vertices $= StringMap.adjust w record{ inStack = False },
                      stack = ws
                    } ts
                in if w == v
                  then record { components $= ((v :: acc) ::) } ts'  -- that's it
                  else createComponent ts' v (w :: acc)

        loop : TarjanState -> List String -> TarjanState
        loop ts [] = ts
        loop ts (w :: ws) =
          loop (
            case StringMap.lookup w ts.vertices of
              Nothing => let ts' = strongConnect ts w in
                case StringMap.lookup w ts'.vertices of
                  Nothing => record { impossibleHappened = True } ts'
                  Just wtv => record { vertices $= StringMap.adjust v record{ lowlink $= min wtv.lowlink } } ts'

              Just wtv => case wtv.inStack of
                False => ts  -- nothing to do
                True => record { vertices $= StringMap.adjust v record{ lowlink $= min wtv.index } } ts
          ) ws

        ts' : TarjanState
        ts' = record {
            vertices  $= StringMap.insert v (TV ts.nextIndex ts.nextIndex True),
            stack     $= (v ::),
            nextIndex $= (1+)
          } ts

    loop : TarjanState -> List String -> List (List String)
    loop ts [] =
      if ts.impossibleHappened
        then []
        else ts.components
    loop ts (v :: vs) =
      case StringMap.lookup v ts.vertices of
        Just _ => loop ts vs  -- done, skip
        Nothing => loop (strongConnect ts v) vs

coreFor : List a -> (a -> Core b) -> Core (List b)
coreFor xs f = Core.traverse f xs

record ModuleInfo where
  constructor MkMI
  name : ModuleName
  outdated : Bool

generateModules : Ref Ctxt Defs -> ClosedTerm -> (outfile : String) -> Core (List ModuleInfo)
generateModules c tm bld = do
  cdata <- getCompileData False Cases tm
  let ndefs = namedDefs cdata
  let ctm = forget (mainExpr cdata)
  let ldefs = lazyDefs ndefs
  let defsByNS = StringMap.fromList $ splitByNS ndefs
  let defDepsRaw = [StringMap.singleton (mlfNS n) (SortedSet.delete (mlfNS n) (nsDef d)) | (n, fc, d) <- ndefs]
  let defDeps = foldl (StringMap.mergeWith SortedSet.union) StringMap.empty defDepsRaw
  let components = reverse $ tarjan defDeps  -- tarjan generates reverse toposort

  -- map each module name / namespace
  -- to the representative from its component
  let nsMapping =
        foldl
          (\nm, modNames => case modNames of
            [] => nm
            mn :: mns =>
              let mlMod = MkMN (foldl min mn mns)
                in foldl (\nm, modName => StringMap.insert modName mlMod nm)
                    nm
                    modNames
          )
          StringMap.empty
          components

  -- generate one module per strongly connected component
  -- start with Builtins, work up to modules with many dependencies
  outdatedModNames <- coreLift $ newIORef SortedSet.empty
  moduleNames <- coreFor components $ \modNames => case modNames of
    [] => throw $ InternalError "empty connected component"
    mn :: mns => do
      let mlModName = MkMN (foldl min mn mns)
      let defs = concatMap (\modName => fromMaybe [] $ StringMap.lookup modName defsByNS) modNames
      let defsMlf = map (mlfDef ldefs nsMapping mlModName) defs
      let code = render " " $ parens (
            text "module"
            $$ indent (
                 mlfRec defsMlf
              $$ text ""
              $$ parens (
                text "export"
                $$ indent (vcat [mlfGlobalVar n | (n, _, _) <- defs])
                )
              )
            )
            $$ text ""
            $$ text "; vim: ft=lisp"
            $$ text ""  -- end with a newline

      -- check if the files need updating
      let codeHashStr = show (hash code) ++ "\n"
      mbPrevHash <- coreLift (readFile (bld </> mlModName.string <.> "hash")) >>= \case
        Left err => pure $ Nothing
        Right h  => pure $ Just h

      isUpToDate <- do
        outdatedMNs <- coreLift $ readIORef outdatedModNames
        let allDeps = concat
              [ fromMaybe SortedSet.empty (StringMap.lookup n defDeps)
              | n <- with Prelude.(::) mn :: mns
              ]
        pure $
          -- hash matches
          (mbPrevHash == Just codeHashStr)
          -- no deps are outdated
          -- we check only direct deps because we're traversing in dep order, anyway
          && (null $ SortedSet.intersection allDeps outdatedMNs)

      if isUpToDate
        then pure (MkMI mlModName False)  -- up to date, nothing to do
        else do
          -- mark all namespaces in this module as outdated
          coreLift $ do
            omns <- readIORef outdatedModNames
            writeIORef outdatedModNames $
              omns <+> SortedSet.fromList (mn :: mns)

          -- write the MLF file
          let fname = bld </> mlModName.string <.> "mlf"
          Right () <- coreLift $ writeFile fname code
            | Left err => throw (FileErr fname err)

          -- update the hash file
          -- write into .hash.tmp, which will be renamed to .hash
          -- once the build succeeds
          let fname = bld </> mlModName.string <.> "hash.tmp"
          Right () <- coreLift $ writeFile fname codeHashStr
            | Left err => throw (FileErr fname err)

          -- write the MLI file
          let mliCode = render " " $
                vcat
                  [ text "val" <++> mlfGlobalName n <++> text ": 'a"
                  | (n, _, _) <- defs
                  ]
          let fname = bld </> mlModName.string <.> "mli"
          Right () <- coreLift $ writeFile fname mliCode
            | Left err => throw (FileErr fname err)

          pure (MkMI mlModName True)

  -- generate the main module
  mainMlf <- pure $ mlfTm ldefs nsMapping (MkMN "Main") ctm
  let code = render " " $ parens (
        text "module"
        $$ indent (
          parens (text "_" <++> mainMlf)
          $$ text ""
          $$ parens (text "export")
          )
        )
        $$ text ""
        $$ text "; vim: ft=lisp"
        $$ text ""  -- end with a newline
  Right () <- coreLift $ writeFile (bld </> "Main.mlf") code
    | Left err => throw (FileErr (bld </> "Main.mlf") err)

  -- write an empty hash file
  Right () <- coreLift $ writeFile (bld </> "Main.hash.tmp") ""
    | Left err => throw (FileErr (bld </> "Main.hash.tmp") err)

  -- write an empty Main.mli
  Right () <- coreLift $ writeFile (bld </> "Main.mli") ""
    | Left err => throw (FileErr (bld </> "Main.mli") err)

  pure $ moduleNames ++ [MkMI (MkMN "Main") True]

firstAvailable : List String -> String -> Core (Maybe String)
firstAvailable [] fname = pure Nothing
firstAvailable (dir :: dirs) fname = do
  let path = dir </> fname
  case !(coreLift $ openFile path Read) of
    Right f => do
      coreLift $ closeFile f
      pure (Just path)
    Left _ => firstAvailable dirs fname

copy : Dirs -> String -> String -> Core ()
copy dirs bld fn =
  firstAvailable dirs.data_dirs fn >>= \case
    Nothing => throw $ InternalError ("idris2-mlf/copy: could not find " ++ fn)
    Just path => do
      0 <- coreLift $ system $ unwords ["cp", path, bld]
        | e => throw (FileErr path (GenericFileError 0))
      pure ()

compileExpr : Ref Ctxt Defs
  -> (tmpDir : String) -> (outputDir : String)
  -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c tmpDir outputDir tm outfile = do
  let bld = tmpDir </> "mlf-" ++ outfile
  Right () <- coreLift $ mkdirAll bld
    | Left err => throw (FileErr bld err)

  -- malfunction does not support libs in another directory
  -- let's just copy all of them
  dirs <- getDirs
  copy dirs bld ("mlf" </> "Rts.cmx")
  copy dirs bld ("mlf" </> "Rts.cmi")
  copy dirs bld ("mlf" </> "Rts.o")
  copy dirs bld ("mlf" </> "rts_c.o")

  modules <- generateModules c tm bld

  let cmd = unwords
        [ " (cd " ++ bld
        -- rebuild only the outdated MLF modules
        , unwords
          [    " && ocamlfind opt -I +threads -g -c " ++ mod.name.string ++ ".mli "
            ++ " && malfunction cmx " ++ mod.name.string ++ ".mlf"
            -- mark the module build as successful
            ++ " && mv " ++ mod.name.string ++ ".hash.tmp " ++ mod.name.string ++ ".hash"
          | mod <- modules
          , mod.outdated
          ]
        -- link it all together
        , "&& ocamlfind opt -thread -package zarith -linkpkg -nodynlink -g "
            ++ "rts_c.o "
            ++ !(findLibraryFile "libidris2_support.a") ++ " "
            ++ "Rts.cmx "
            ++ unwords [mod.name.string ++ ".cmx" | mod <- modules]
            ++ " -o ../" ++ outfile
        , ")"
        ]

  coreLift $ putStrLn cmd
  ok <- coreLift $ system cmd
  if ok == 0
    then pure (Just (outputDir </> outfile))
    else pure Nothing

executeExpr : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
executeExpr c tmpDir tm
    = do outn <- compileExpr c tmpDir tmpDir tm "_tmp_mlf"
         case outn of
              -- TODO: on windows, should add exe extension
              Just outn => map (const ()) $ coreLift $ system outn
              Nothing => pure ()

main : IO ()
main = mainWithCodegens
  [ ("mlf", MkCG compileExpr executeExpr Nothing Nothing)
  ]
