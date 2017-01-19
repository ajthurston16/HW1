{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs #-}

{-# OPTIONS -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, zip, reverse, concat)
import Test.HUnit
import qualified Data.List as List
import qualified Data.Char as Char

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testWeather,
                               testSoccer ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z =
  x && (y || z)


tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]


arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
  (b*f-c*e, c*d-a*f, a*e-b*d)


tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]


reverse :: [a] -> [a]
reverse l  = reverseAux l [] where
  reverseAux l acc =
    case l of
      x : xs -> reverseAux xs (x : acc)
      [] -> acc


treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

zap funs args = g 0 funs args where
  g n xs ys = if n == length funs || n == length args then [] else
          (xs !! n) (ys !! n) : g (n + 1) xs ys

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip,
   tmapMaybe, ttranspose, tconcat, tcountSub, tsplitBy]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

intersperse :: a -> [a] -> [a]
intersperse c l =
  case l of
    x : [] -> x : []
    x : xs -> x : c : intersperse c xs
    _ -> []
--intersperseAux c l [] where
--  intersperseAux c l acc =
--    case l of
 --     x : xs -> x : xs
      --x : xs -> intersperseAux c xs (acc : x : c)
--      _ -> acc : x


tintersperse :: Test
tintersperse = "intersperse" ~: assertFailure "testcase for intersperse"


-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

invert :: [(a, b)] -> [(b, a)]
invert l =
  case l of
    (a, b) : rest -> (b, a) : invert rest
    _ -> []

tinvert :: Test
tinvert = "invert" ~: assertFailure "testcase for invert"


-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []
--
-- takeWhile is a Prelude function

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p l =
  case l of
    x : xs -> if p x then x : takeWhile p xs else []
    _ -> []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: assertFailure "testcase for takeWhile"


-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
--
-- find is defined in Data.List

find :: (a -> Bool) -> [a] -> Maybe a
find pred lst =
  case lst of
    x : xs -> if pred x then Just x else find pred xs
    _ -> Nothing

tfind :: Test
tfind = "find" ~: assertFailure "testcase for find"


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
--
-- all is a prelude function
all :: (a -> Bool) -> [a] -> Bool
all pred lst =
  case lst of
    x : xs -> if pred x then True && all pred xs else False
    _ -> True

tall :: Test
tall = "all" ~: assertFailure "testcase for all"


-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the Prelude


map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xLst yLst =
  case (xLst, yLst) of
    (x : xs, y : ys) -> f x y : map2 f xs ys
    _ -> []

tmap2 :: Test
tmap2 = "map2" ~: assertFailure "testcase for map2"

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]
--
-- zip is defined in the prelude


zip :: [a] -> [b] -> [(a, b)]
zip xLst yLst =
  case (xLst, yLst) of
     (x : xs, y : ys) -> (x, y) : zip xs ys
     _ -> []

tzip :: Test
tzip = "zip" ~: assertFailure "testcase(s) for zip"

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--
-- transpose is defined in Data.List

-- transpose :: [[a]] -> [[a]]
-- transpose lst =
--   case lst of
--     x1 : x2 : xs ->


ttranspose :: Test
ttranspose = "transpose" ~: assertFailure "testcase for transpose"

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.


concat :: [[a]] -> [a]
concat = concatAux lst [] where
  concatAux [] _ = []                   -- Stop when at end of outer lists
  concatAux (x : xs) acc =
    case x of
      elt : elts -> concatAux (elts : xs) (acc : x)
      _ -> concatAux xs acc               -- Move to next inner list

-- concat lst =
--   case lst of
--     x : xs -> case x of
--                 elt : elts -> elt : concat [elts]
--                 _ -> x
--     _ -> []

tconcat :: Test
tconcat = "concat" ~: assertFailure "testcase for concat"

-- mapMaybe

-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

mapMaybe :: (a -> Maybe a) -> [a] -> [a]
mapMaybe f lst =
  case lst of
    x : xs -> case f x of
                Just val -> val : mapMaybe f xs
                Nothing -> mapMaybe f xs
    _ -> []

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: assertFailure "testcase for mapMaybe"

-- countSub sub str

-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

-- countSub :: [Char] -> [Char] -> Int
-- countSub sub [] = 0
-- countSub sub (x : xs)
--   | trimTail str sub == sub = 1 + countSub sub xs
--   | otherwise = countSub sub xs

--   where trimTail sub str =
--     case (sub, str) of
--       (x, y : ys) -> y : trimTail x ys
--       _ -> []


tcountSub :: Test
tcountSub = "countSub" ~: assertFailure "testcase for countSub"

-- splitBy pred lst
--
-- Divide the list into sections delimited by the given predicate, which do not
-- appear in the output below.
--    for example,
--      splitBy isSpace "four score and seven years" returns
--            ["four","score","and","seven","years"]
--      splitBy isSpace "" returns []

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace  _  = False

-- splitBy :: (a -> Bool) -> [a] -> [[a]]
-- splitBy pred lst = splitByAux pred lst [] where
--   splitByAux pred [] _ = []
--   splitByAux pred (x : xs) acc
--      | pred x = acc : splitByAux pred xs []
--      | otherwise = splitByAux pred xs (acc : [x])

tsplitBy :: Test
tsplitBy = "splitBy" ~: assertFailure "testcase for splitBy"

--------------------------------------------------------------------------------

-- Part One: Weather Data

weather :: String -> String
weather str = error "unimplemented"


weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "weather.dat"
  putStrLn (weather str)

readInt :: String -> Int
readInt = read

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "weather.dat"
                              weather str @?= "9"

--------

-- Part Two: Soccer League Table

soccer :: String -> String
soccer = error "unimplemented"


soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "football.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "football.dat"
  soccer str @?= "Arsenal"

-- Part Three: DRY Fusion

weather2 :: String -> String
weather2 = undefined

soccer2 :: String -> String
soccer2 = undefined

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "Fill in your answer here"

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "Fill in your answer here"

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "Fill in your answer here"


