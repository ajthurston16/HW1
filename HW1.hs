-- Authors: Guillermo Gutierrez, Alex Thurston
-- CIS 552 HW1
-- Date Due: 23 January 2017

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs #-}

{-# OPTIONS -fdefer-type-errors -Werror #-}

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

zap :: [t -> a] -> [t] -> [a]
zap funs args =
    case (funs, args) of
      (f : fs, ar : ars) -> f ar : zap fs ars
      _ -> []

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
intersperse c lst =
  case lst of
    [x] -> [x]
    x : xs -> x : c : intersperse c xs
    _ -> []

tintersperse :: Test
tintersperse = "intersperse" ~:
  TestList[ intersperse ',' "abcde" ~?= "a,b,c,d,e",
            intersperse ',' "" ~?= "",
            intersperse ',' "a" ~?= "a"]

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

invert :: [(a, b)] -> [(b, a)]
invert lst =
  case lst of
    (a, b) : rest -> (b, a) : invert rest
    _ -> []

tinvert :: Test
tinvert = "invert" ~:
  TestList[ invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")],
            invert ([] :: [(Int,Char)]) ~?= [],
            invert [(1, "c")] ~?= [("c", 1)]]

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
takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~:
  TestList[ takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1, 2],
            takeWhile (> -9) [1,2,3] ~?= [1,2,3],
            takeWhile (< 0) [1,2,3] ~?= [],
            takeWhile (< 3) [] ~?= []]


-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
--
-- find is defined in Data.List

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find pred (x : xs) = if pred x then Just x else find pred xs

tfind :: Test
tfind = "find" ~:
  TestList[ find odd [0,2,3,4] ~?= Just 3,
            find odd [2, 4, 6] ~?= Nothing,
            find odd [] ~?= Nothing]


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
--
-- all is a prelude function
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all pred (x : xs) = pred x && all pred xs

tall :: Test
tall = "all" ~:
  TestList[ all odd [1, 3, 5] ~?= True,
            all odd [2, 4, 6] ~?= False,
            all odd [1, 3, 6] ~?= False,
            all odd [] ~?= True,
            all even [2] ~?= True]

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
tmap2 = "map2" ~:
  TestList[ map2 (+) [1, 2, 3] [4, 5, 6] ~?= [5, 7, 9],
            map2 (+) [1, 2, 3, 5] [4, 5, 6] ~?= [5, 7, 9],
            map2 (+) [1, 2, 3] [4, 5, 6, 10] ~?= [5, 7, 9],
            map2 (+) [1, 2, 3] [] ~?= []]

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
tzip = "zip" ~:
  TestList[ zip [1,2] [True] ~?= [(1,True)],
            zip [1,2] [True, False] ~?= [(1,True), (2, False)],
            zip ([] :: [Int]) [True] ~?= [],
            zip [1, 2] [3, 4] ~?= [(1, 3), (2, 4)]]

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

-- let (x, xs) = transposeSplit lst in... x :

transpose :: [[a]] -> [[a]]
transpose [[]] = [[]]
transpose lst =
  case transposeAux lst of
    (_, []) -> []
    (x, xs) -> if checkSize xs then x : transpose xs else [x]


-- | Zip the heads and tails of each inner list into separate lists

transposeAux :: [[a]] -> ([a], [[a]])
transposeAux ((x : xs) : rest) = (x : res1, xs : res2)
              where (res1, res2) = transposeAux rest
transposeAux _ = ([], [])


-- | Return false if any inner list is empty

checkSize :: [[a]] -> Bool
checkSize lst =
  case lst of
    ([] : xs) -> False
    (x  : xs) -> checkSize xs
    _ -> True

ttranspose :: Test
ttranspose = "transpose" ~:
  TestList[ transpose [[1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9]] ~?=
                      [[1, 4, 7],
                       [2, 5, 8],
                       [3, 6, 9]],
            transpose [[1, 2, 3], [4, 5]] ~?= [[1, 4], [2, 5]],
            transpose [[1, 2], [3, 4, 5]] ~?= [[1, 3], [2, 4]],
            transpose [[] :: [Int]] ~?= [[] :: [Int]]
  ]

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

concat :: [[a]] -> [a]
concat lst =
  case lst of
    [x] : rest -> x : concat rest
    (x : xs) : rest -> x : concat (xs : rest)
    _ -> []

tconcat :: Test
tconcat = "concat" ~:
  TestList[ concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]] ~?= [1, 2, 3, 4, 5, 6, 7,
                     8, 9],
            concat [[1, 2], [3, 4, 5]] ~?= [1, 2, 3, 4, 5],
            concat [[1]] ~?= [1],
            concat [[] :: [Int]] ~?= []]
-- mapMaybe

-- Map a partial function over all the elements of the list
-- for example:
--    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

mapMaybe :: (a -> Maybe a) -> [a] -> [a]
mapMaybe _ [] = []
mapMaybe f (x : xs) =
  case f x of
    Just val -> val : mapMaybe f xs
    Nothing  -> mapMaybe f xs

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~:
  TestList[ mapMaybe root [0.0, -1.0, 4.0] ~?= [0.0,2.0],
            mapMaybe root [] ~?= [],
            mapMaybe root [-1.0, -2.0] ~?= ([] :: [Double])]

-- countSub sub str

-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

countSub :: String -> String -> Int
countSub [] _ = 0
countSub _ [] = 0
countSub sub (x : xs)
    | strSlice (x : xs) (strLen sub) == sub = countSub sub xs + 1
    | otherwise = countSub sub xs


-- | Return the first i chars from a nonempty string

strSlice :: String -> Int -> String
strSlice [] _ = []
strSlice (x : xs) i
    | i == 0 = []
    | otherwise = x : strSlice xs (i - 1)


-- | Get the length of a string

strLen :: String -> Int
strLen [] = 0
strLen (x : xs) = 1 + strLen xs

tcountSub :: Test
tcountSub = "countSub" ~:
  TestList[ countSub "aa" "aaa" ~?= 2,
            countSub "aa" "ababtsdasqdsdgdaca" ~?= 0,
            countSub "aaaaa" "aaaaa" ~?= 1,
            countSub "" "aas" ~?= 0,
            countSub "aa" "" ~?= 0
          ]

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


-- | Split a list into two lists at the point where the predicate is true

buildSubStr :: (a -> Bool) -> [a] -> ([a] , [a])
buildSubStr _ [] = ([], [])
buildSubStr pred (x : xs)
    | pred x = ([], xs)
    | otherwise = (x : res1 , res2) where (res1 , res2) = buildSubStr pred xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy pred lst =
  case buildSubStr pred lst of
    ([], []) -> []
    ([], remainingInput)          -> splitBy pred remainingInput
    (finishedSub, remainingInput) -> finishedSub : splitBy pred remainingInput


tsplitBy :: Test
tsplitBy = "splitBy" ~:
  TestList[splitBy isSpace "Four score and seven years" ~?= ["Four", "score",
                           "and", "seven", "years"],
           splitBy isSpace "" ~?= ([] :: [String]),
           splitBy isSpace "    " ~?= ([] :: [String]),
           splitBy isSpace " Four     score and seven years" ~?=
                          ["Four", "score", "and", "seven", "years"]
          ]

--------------------------------------------------------------------------------

-- Part One: Weather Data

weather :: String -> String
weather str = getMinWeather " " 1000 (getColWeather (drop 2 (lines str)))


-- | Return a string corresponding to the tuple with the minimum difference

getMinWeather :: String -> Int -> [(String, Int, Int)] -> String
getMinWeather sol _ [] = sol
getMinWeather sol minSoFar ((rowNum, hi, lo) : rest)
    | hi - lo < minSoFar = getMinWeather rowNum (hi - lo) rest
    | otherwise = getMinWeather sol minSoFar rest



-- | Read two valid numbers from each line of text and construct a tuple

getColWeather :: [String] -> [(String, Int, Int)]
getColWeather (x : xs)
    | isValidNum hi && isValidNum lo = (day, readInt hi,
                                        readInt lo) : getColWeather xs
    | otherwise = getColWeather xs
      where day : hi : lo : rest = splitBy isSpace x
getColWeather _ = []

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "weather.dat"
  putStrLn (weather str)

readInt :: String -> Int
readInt = read

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "weather.dat"
                              weather str @?= "14"

--------

-- Part Two: Soccer League Table

soccer :: String -> String
soccer str = getMinSoc " " 1000 (getColSoc (dropElemsSoc [22,18,0] (lines str)))


-- | This function drops the elements in sequential order. Since it uses the
--   splitAt function, once it drops an element, the indexing will change
--   The easiest solution is to provide the elements to be dropped in descending
--   order.

dropElemsSoc :: [Int] -> [a] -> [a]
dropElemsSoc _ [] = []
dropElemsSoc [] lst = lst
dropElemsSoc (n : ns) lst
    | n >= length lst = dropElemsSoc ns lst
    | otherwise = let (front, x : back) = splitAt n lst in
                  dropElemsSoc ns (front ++ back)


-- | Return a string corresponding to the tuple with the absolute minimum diff

getMinSoc :: String -> Int -> [(String, Int, Int)] -> String
getMinSoc sol _ [] = sol
getMinSoc sol minSoFar ((rowNum, hi, lo) : rest)
    | abs(hi - lo) < minSoFar = getMinSoc rowNum (abs(hi - lo)) rest
    | otherwise = getMinSoc sol minSoFar rest


-- | Read two valid numbers from each line of text and construct a tuple

getColSoc :: [String] -> [(String, Int, Int)]
getColSoc [] = []
getColSoc (x : xs)
    | isValidNum hi && isValidNum lo = (day, readInt hi,
                                        readInt lo) : getColSoc xs
    | otherwise = getColSoc xs
      where day : hi : lo : rest = dropElemsSoc [9,7,5,4,3,2,0] (splitBy
                                                                 isSpace x)

soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "football.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "football.dat"
  soccer str @?= "Aston_Villa"

-- Part Three: DRY Fusion

isValidNum :: String -> Bool
isValidNum = foldr ((&&) . Char.isDigit) True


-- | This function drops the elements in sequential order. Since it uses the
--   splitAt function, once it drops an element, the indexing will change
--   The easiest solution is to provide the elements to be dropped in descending
--   order.

dropElems :: [Int] -> [a] -> [a]
dropElems _ [] = []
dropElems [] lst = lst
dropElems (n : ns) lst
    | n >= length lst = dropElems ns lst
    | otherwise = let (front, x: back) = splitAt n lst in
                  dropElems ns (front ++ back)


-- | Read two valid numbers from each line of text and construct a tuple

getCol :: [Int] -> [String] -> [(String, Int, Int)]
getCol _ [] = []
getCol dropLst (x : xs)
    | isValidNum firstInt && isValidNum secondInt = (name, readInt firstInt,
                                        readInt secondInt) : getCol dropLst xs
    | otherwise =  getCol dropLst xs
      where name : firstInt : secondInt : _ = dropElems dropLst (splitBy
                                                                 isSpace x)


-- | Return a string corresponding to the tuple with the absolute minimum diff

getMin :: String -> Int -> [(String, Int, Int)] -> String
getMin sol _ [] = sol
getMin sol minSoFar ((name, firstInt, secondInt) : rest)
    | abs(firstInt - secondInt) < minSoFar = getMin name (abs(
                                              firstInt - secondInt)) rest
    | otherwise = getMin sol minSoFar rest


weather2 :: String -> String
weather2 str = getMin " " 1000 (getCol [] (dropElems [1,0] (lines str)))


soccer2 :: String -> String
soccer2 str = getMin " " 1000 (getCol [9,7,5,4,3,2,0] (
                               dropElems [22,18,0] (lines str)))

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "Our original solutions depended removing irrelevant rows and \
               \columns to find the data needed to calculate the solution. \
               \Most of this parsing was contained within a single helper \
               \function, while the actual calculation of the solution was \
               \done in another.  This design choice helped when we were \
               \refactoring, since only the parsing function needed \
               \serious changes to accommodate different file formats. \
               \Although implementing this shared function was somewhat \
               \difficult, it was good that this was the only major \
               \optimization needed."

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "Yes. Our basic approach for the first problem was to split \
               \the original string into lines, then split these lines into \
               \columns.  For the weather program, we could then keep the \
               \first three (adjacent) columns and derive the solution from \
               \those. We wished to apply the same process to the second \
               \program, though we found that it became more difficult to \
               \parse lines when the appropriate columns were no longer \
               \adjacent and at the beginning of each line.  We found \
               \ourselves parsing a more complicated file format so that our \
               \other helper functions from the first program would also work \
               \in the second."

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "It is probably not always a good thing, and the readability \
                \did suffer somewhat.  Our original soccer program managed to \
                \drop unnecessary rows and columns in a concise manner with \
                \simple pattern matches, but the refactored version required a \
                \less concise series of function calls. The refactored \
                \version may ultimately be more maintainable, however, since \
                \it can be extended to a wider range of file formats that \
                \could appear in the future"


