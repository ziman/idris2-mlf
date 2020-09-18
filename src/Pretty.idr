module Pretty

import Data.List
import Data.Strings

%default total

export
data Doc : Type where
  Text : String -> Doc
  Vcat : List Doc -> Doc
  Hang : Doc -> Doc -> Doc
  Indent : Doc -> Doc

public export
interface Pretty a where
  pretty : a -> Doc

export
Semigroup Doc where
  (<+>) = Hang

export
Monoid Doc where
  neutral = Text ""

export
text : String -> Doc
text = Text

export
show : Show a => a -> Doc
show = Text . show

infixr 2 $$
export
($$) : Doc -> Doc -> Doc
($$) (Vcat xs) (Vcat ys) = Vcat (xs ++ ys)
($$) x (Vcat ys) = Vcat (x :: ys)
($$) (Vcat xs) y = Vcat (xs ++ [y])
($$) x y = Vcat [x,y]

export
vcat : List Doc -> Doc
vcat = Vcat

export
vsep : List Doc -> Doc
vsep = vcat . intersperse neutral

export
punctuate : Doc -> List Doc -> Doc
punctuate sep = concat . intersperse sep

export
hsep : List Doc -> Doc
hsep = punctuate (text " ")

infixl 6 <++>
export
(<++>) : Doc -> Doc -> Doc
(<++>) x y = x <+> text " " <+> y

export
indent : Doc -> Doc
indent = Indent

export
indentBlock : List Doc -> Doc
indentBlock = indent . vcat

export
parens : Doc -> Doc
parens d = text "(" <+> d <+> text ")"

export
brackets : Doc -> Doc
brackets d = text "[" <+> d <+> text "]"

export
braces : Doc -> Doc
braces d = text "{" <+> d <+> text "}"

private
tcMap : (a -> b) -> List a -> List b
tcMap f = go []
  where
    go : List b -> List a -> List b
    go acc [] = reverse acc
    go acc (x :: xs) = go (f x :: acc) xs

private
hang : String -> List String -> List String -> List String
hang ind [] ys = ys
hang ind xs [] = xs
hang ind [x] (y :: ys) = (x ++ y) :: tcMap (ind++) ys
hang ind (x :: xs) ys = x :: hang ind xs ys

private
render' : String -> Doc -> List String
render' ind (Text s) = [s]
render' ind (Vcat ls) = assert_total $ concatMap (render' ind) ls
render' ind (Hang x y) = hang ind (render' ind x) (render' ind y)
render' ind (Indent x) = tcMap (ind++) $ render' ind x

export
render : String -> Doc -> String
render ind = fastAppend . tcMap (++"\n") . render' ind

export
Show Doc where
  show = render "  "

export
prettyShow : Pretty a => a -> String
prettyShow = render "  " . pretty
