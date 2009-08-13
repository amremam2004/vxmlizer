This module is taken from the lecture notes of
Functional Programming Class
Portland State University
Spring 2009
Mark P Jones and Tim Sheard


>module Treedot2 where

Type Classes:
-------------
What distinguishes "tree types" from other types?  In my
opinion, a key distinction is that:

  a value of a tree type can have zero or more subtrees

And, for any given tree type, there's really only one sensible
way to do this.

> class Tree t where
>   subtrees :: t -> [t]

For example:

> data BinTree a   = Leaf a
>                  | BinTree a :^: BinTree a
>                    deriving Show

> example :: BinTree Int
> example  = l :^: r
>  where l = p :^: q
>        r = s :^: t
>        p = Leaf 1 :^: t
>        q = s :^: Leaf 2
>        s = Leaf 3 :^: Leaf 4
>        t = Leaf 5 :^: Leaf 6

> instance Tree (BinTree a) where
>   subtrees (Leaf x)   = []
>   subtrees (l :^: r)  = [l, r]

> data LabTree l a = Tip a
>                  | LFork l (LabTree l a) (LabTree l a)

> instance Tree (LabTree l a) where
>   subtrees (Tip a)       = []
>   subtrees (LFork s l r) = [l, r]

> data STree a     = Empty
>                  | Split a (STree a) (STree a)

> instance Tree (STree a) where
>   subtrees Empty = []
>   subtrees (Split s l r) = [l, r]

> data RoseTree a  = Node a [RoseTree a]

> instance Tree (RoseTree a) where
>   subtrees (Node x cs) = cs

> data Expr        = Var String
>                  | IntLit Int
>                  | Plus Expr Expr
>                  | Mult Expr Expr

> instance Tree Expr where
>   subtrees (Var s)    = []
>   subtrees (IntLit n) = []
>   subtrees (Plus l r) = [l, r]
>   subtrees (Mult l r) = [l, r]

Now we can define a whole bunch of interesting functions on
trees in a very generic way:

> depth  :: Tree t => t -> Int
> depth   = (1+) . foldl max 0 . map depth . subtrees

> size   :: Tree t => t -> Int
> size    = (1+) . sum . map size . subtrees

> paths               :: Tree t => t -> [[t]]
> paths t | null br    = [ [t] ]
>         | otherwise  = [ t:p | b <- br, p <- paths b ]
>           where br = subtrees t

> dfs    :: Tree t => t -> [t]
> dfs t   = t : concat (map dfs (subtrees t))

In each of these operations, the "Tree t => ..." portion of
the type indicates that the function works for any type t,
so long as t is a Tree type (i.e., so long as there is a
definition of subtrees for t).

To be able to convert trees into dot format, we need the
nodes to be labelled with strings.  Not all trees are
labelled in this way, so we create a subclass:

> class Tree t => LabeledTree t where
>   label :: t -> String

> instance LabeledTree (BinTree String) where
>   label (Leaf x)   = x
>   label (l :^: r)  = ""

> instance LabeledTree (BinTree Int) where
>   label (Leaf x)   = show x
>   label (l :^: r)  = ""


> instance LabeledTree (LabTree String String) where
>   label (Tip a)       = a
>   label (LFork s l r) = s

> instance LabeledTree (STree String) where
>   label Empty         = ""
>   label (Split s l r) = s

> instance LabeledTree (RoseTree String) where
>   label (Node x cs) = x

> instance LabeledTree Expr where
>   label (Var s)    = s
>   label (IntLit n) = show n
>   label (Plus l r) = "+"
>   label (Mult l r) = "*"

This gives us the machinery that we need to create a generic
version of the toDot function:

> toDot :: LabeledTree t => t -> String -> IO ()
> toDot t dst = writeFile dst
>                ("digraph tree {\n"
>                 ++ semi (nodeTree [] t)
>                 ++ "}\n")
>  where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""

> type Path    = [Int]
> type NodeId  = String

> showPath      :: Path -> String
> showPath p     = "\"" ++ show p ++ "\""

> nodeTree    :: LabeledTree t => Path -> t -> [String]
> nodeTree p t = [ showPath p ++ " [label=\"" ++ label t ++ "\"]" ]
>             ++ concat (zipWith (edgeTree p) [1..] (subtrees t))

> edgeTree      :: LabeledTree t => Path -> Int -> t -> [String]
> edgeTree p n c = [ showPath p ++ " -> " ++ showPath p' ] ++ nodeTree p' c
>                  where p' = n : p

Not all the trees that we want to use are included in
the class of LabeledTrees because the values they store
are not strings.

We can address this by mapping the show function over a
BinTree using our old friend:

> mapTree            :: (a -> b) -> BinTree a -> BinTree b
> mapTree f (Leaf x)  = Leaf (f x)
> mapTree f (l :^: r) = mapTree f l :^: mapTree f r

But what about other types of trees?  How can we convert
elements of other tree types into strings.  Again, we need
a mapping operation ... and this time it turns out that
there is a class for just this purpose that is built in to
the standard Prelude:

  class Functor f where
    fmap :: (a -> b) -> f a -> f b

  instance Functor [] where ...
  instance Functor Maybe where ...

I'm not showing actual code here (because that would conflict
with the definitions of this class and its instances in the
prelude).  But we can add instances for the different tree
types defined previously:

> instance Functor BinTree where
>   fmap f (Leaf x)   = Leaf (f x)
>   fmap f (l :^: r)  = fmap f l :^: fmap f r

> instance Functor (LabTree l) where
>   fmap f (Tip a)       = Tip (f a)
>   fmap f (LFork s l r) = LFork s (fmap f l) (fmap f r)

> instance Functor STree where
>   fmap f Empty         = Empty
>   fmap f (Split s l r) = Split (f s) (fmap f l) (fmap f r)

> instance Functor RoseTree where
>   fmap f (Node x cs) = Node (f x) (map (fmap f) cs)

[Question: Why is there no instance for Functor Expr?]

