import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)

-- AVL: an AVL tree suitable for use as a set, map, or maybe other things.
data AVL a =
    Empty
  | Node (AVL a) a Int (AVL a) -- left-branch, value, height, right-branch
  deriving (Show)

-- node creates an AVL Node and sets the proper height automatically
node :: AVL a -> a -> AVL a -> AVL a
node l v r = Node l v (1 + max (height l) (height r)) r

-- leaf creates a leaf node with value v
leaf :: a -> AVL a
leaf v = Node Empty v 0 Empty

-- value gets the value at the root of the tree
value :: AVL a -> Maybe a
value Empty = Nothing
value (Node _ v _ _) = Just v

-- height gets the height of the tree
height :: AVL a -> Int
height Empty = -1
height (Node _ _ h _) = h

-- items converst the tree to a list of its node values, in order from left to right
items :: AVL a -> [a]
items Empty = []
items (Node l v _ r) = items l ++ [v] ++ items r

-- bf gets the balance factor of the tree
bf :: AVL a -> Int
bf Empty = 0
bf (Node l _ _ r) = height r - height l

-- avlProperty asks "does the AVL propery hold for this tree?"
avlProperty :: AVL a -> Bool
avlProperty t = any (== (bf t)) [-1, 0, 1]

data Dir = L | R deriving (Show, Eq)

-- heavier asks "which side of t is heavier"? Crashes if t is balanced
heavySide :: AVL a -> Dir
heavySide t =
    let b = bf t
    in if b < 0 then L else if b > 0 then R else error "node is balanced"

data AVLLean = Balanced | LeftInside | RightInside | LeftOutside | RightOutside deriving (Show, Eq)

-- avlLean is used to determine how to balance a tree. See AVLLean
avlLean :: AVL a -> AVLLean
avlLean Empty = Balanced
avlLean t@(Node left val _ right) =
    case bf t of
        0 -> Balanced
        -1 -> Balanced
        1 -> Balanced
        b | b < 0 -> if heavySide left == L then LeftOutside else LeftInside
        _ -> if heavySide right == R then RightOutside else RightInside

-- SEARCHING --------------
pathTo :: AVL a -> (a -> Ordering) -> [Dir]
pathTo Empty p = []
pathTo n@(Node l v _ r) p =
    case p v of
        EQ -> []
        GT -> -- the value being search for is greater
            R : pathTo r p
        LT -> -- the value being search for is lesser
            L : pathTo l p

get :: (a -> Ordering) -> AVL a -> Maybe a
get p Empty = Nothing
get p (Node l v _ r) =
    case p v of
        EQ -> Just v
        GT -> get p r
        LT -> get p l

-- INSERTING -------------------
insert :: AVL a -> a -> (a -> Ordering) -> AVL a
insert n u p = insertHelper n u (pathTo n p)

insertHelper :: AVL a -> a -> [Dir] -> AVL a
insertHelper Empty u _ = leaf u
insertHelper (Node l v h r) u [] = node l u r
insertHelper (Node l v h r) u (R:ds) = balanced $ node l v (insertHelper r u ds)
insertHelper (Node l v h r) u (L:ds) = balanced $ node (insertHelper l u ds) v r

-- Balancing:
balanced Empty = Empty
balanced t@(Node left val _ right) =
    case avlLean t of
        Balanced -> t
        LeftOutside -> rotateRight t
        RightOutside -> rotateLeft t
        LeftInside -> rotateRight (node (rotateLeft left) val right)
        RightInside -> rotateLeft (node left val (rotateRight right))

rotateRight Empty = error "cannot rotate Empty"
rotateRight (Node (Node lleft lval _ lright) val _ right) =
    node lleft lval (node lright val right)

rotateLeft Empty = error "cannot rotate Empty"
rotateLeft (Node left val _ (Node rleft rval _ rright)) =
    node (node left val rleft) rval rright

-- UTIL --------------------------------
-- `pretty` prints the tree in a human-readable format
pretty :: (Show a) => AVL a -> String
pretty Empty = "Empty"
pretty (Node Empty v 0 Empty) = "Node[" ++ show v ++ "]"
pretty (Node l v h r) =
    "Node[" ++ show v ++ "] -- height: " ++ show h ++ "\n"
    ++ indent (pretty l)
    ++ indent (pretty r)

-- note: the result of indent ends with a newline
indent str = unlines (map ("    "++) (lines str))

-- UTIL FOR MAPS -----------------------------
pairCompare :: (Ord k) => k -> (k, v) -> Ordering
pairCompare targetKey (k, v) = compare targetKey k

mset :: (Ord k) => k -> v -> AVL (k, v) -> AVL (k, v)
mset key val t = insert t (key, val) (pairCompare key)

mget :: (Ord k) => k -> AVL (k, v) -> Maybe v
mget key t = snd <$> get (pairCompare key) t

assocToMap :: (Ord k) => [(k, v)] -> AVL (k, v)
assocToMap [] = Empty
assocToMap ((k,v):rest) = mset k v (assocToMap rest)

-- UTIL FOR SETS -----------------------------
shas :: (Ord v) => v -> AVL v -> Bool
shas v t = isJust $ get (compare v) t

sins :: (Ord v) => v -> AVL v -> AVL v
sins v t = insert t v (compare v)

listToSet :: (Ord a) => [a] -> AVL a
listToSet [] = Empty
listToSet (x:xs) = sins x (listToSet xs)

-- EXAMPLE -------------------------------
pt :: (Show a) => AVL a -> IO ()
pt = putStrLn . pretty

rot13 :: [(Char, Char)]
rot13 =
    let alpha = ['A'..'Z']
    in zip alpha (drop 13 alpha ++ take 13 alpha)

translate "" m = ""
translate (c:cs) m = fromMaybe '?' (mget (toUpper c) m) : translate cs m

main = do
    putStrLn "Set of integers from 1..10:"
    pt $ listToSet [1..10]
    putStrLn "Map for a scaling by 2:"
    pt $ assocToMap (zip [1..10] (map (*2) [1..10]))
    putStrLn "Rot-13 map:"
    pt $ assocToMap rot13
    putStrLn ("What a secret message: " ++ translate "waow" (assocToMap rot13))
