module W6 where

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List

-- Week 6: Monads

------------------------------------------------------------------------------
-- Ex 1: Here's the ?> chaining operator from the lecture:

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing   -- In case of failure, propagate failure
Just x  ?> f = f x       -- In case of sucess, run the next computation

-- Your task is to help implement the function readName that given a
-- string like "Forename Surname" produces the pair ("Forename",
-- "Surname"). readName should fail (return Nothing) in the following
-- cases:
--
--   1. the input string doesn't contain a space
--   2. both the names are the same
--   3. one of the names doesn't start with a capital letter
--
-- The function readNames has already been implemented using ?>. You
-- need to define the functions split, checkNumber and checkCapitals
-- so that readNames works correctly.

-- DO NOT touch this definition!
readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkDuplicate
  ?>
  checkCapitals

-- split should split a string into two words. If the input doesn't
-- contain a space, Nothing should be returned
--
-- (NB! There are obviously other corner cases like the inputs " " and
-- "a b c", but you don't need to worry about those here)
split :: String -> Maybe (String,String)
split s = if length strArr /= 2 then Nothing else Just (head strArr, last strArr)
  where strArr = words s

-- checkDuplacate should take a pair of two strings and return Nothing
-- if they are the same. Otherwise the strings are returned.
checkDuplicate :: (String, String) -> Maybe (String, String)
checkDuplicate (for,sur) = if for == sur then Nothing else Just (for, sur)

-- checkCapitals should take a pair of two strings and return them
-- unchanged if both start with a capital letter. Otherwise Nothing is
-- returned.
checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (for,sur) = if not (c for) || not (c sur) then Nothing else Just (for, sur)
  where c w = isUpper $ head w

------------------------------------------------------------------------------
-- Ex 2: implement a function myDrop that works just like drop, but
--   1. the arguments are of types Maybe Int and Maybe [a]
--   2. if either of the arguments is Nothing, Nothing is returned
--   3. if the Int is larger than the length of the list or negative,
--      Nothing is returned
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- DO NOT use pattern matching for Maybe.
--
-- Examples:
--  myDrop (Just 1) (Just [5,6,7])
--    ==> Just [6,7]
--  myDrop Nothing (Just [5,6,7])
--    ==> Nothing
--  myDrop (Just 2) Nothing
--    ==> Nothing
--  myDrop (Just 4) (Just [5,6,7])
--    ==> Nothing
myDrop :: Maybe Int -> Maybe [a] -> Maybe [a]
myDrop mi ml = do
  i <- mi
  list <- ml
  when (i > length list || i < 0) Nothing
  return $ drop i list

------------------------------------------------------------------------------
-- Ex 3: given a list of values and a list of indices, return the sum
-- of the values in the given indices. You should fail if any of the
-- indices is too large or too small.
--
-- Use the Maybe monad, i.e. the >>= operator or do-notation.
--
-- DO NOT use pattern matching for Maybe.
--
-- Hint! implementa a function safeIndex :: [a] -> Int -> Maybe a
--
-- Examples:
--  selectSum [2,7,5,3,9] [0,2,4]
--    Just 16
--  selectSum [2,7,5,3,9] [0,2,5]
--    Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (x:xs) n = safeIndex xs (n-1)

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = do
  asd <- mapM (safeIndex xs) is
  return $ sum asd
------------------------------------------------------------------------------
-- Ex 4: below you'll find the Logger monad from the lectures.
--
-- Your task is to implement a function binom that computes binomial
-- coefficients recursively with the following formulae:
--
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--   B(n,k) = B(n-1,k-1) + B(n-1,k)
--
-- Every call to the function should be logged as "B(n,k)".
-- Invocations should be logged in execution order.
--
-- Examples:
--   binom 0 0 ==> Logger ["B(0,0)"] 1
--   binom 0 7 ==> Logger ["B(0,7)"] 0
--   binom 1 1 ==> Logger ["B(0,0)","B(0,1)","B(1,1)"] 1
--   binom 2 2 ==> Logger ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"] 1

data Logger a = Logger [String] a
  deriving Show

instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

-- Disregard this instance. In recent versions of the Haskell standard
-- library, all Monads must also be Applicative. This course doesn't
-- really cover Applicative.
instance Applicative Logger where
  pure = return
  (<*>) = ap

msg :: String -> Logger ()
msg s = Logger [s] ()

-- Implement this:
binom :: Integer -> Integer -> Logger Integer
binom n 0 = msg ("B("++show n++",0)") >> return 1
binom 0 k = msg ("B(0,"++show k++")") >> return 0
binom n k = do
  x <- binom (n-1) (k-1)
  y <- binom (n-1) k
  msg ("B("++show n++","++show k ++")")
  return (x+y)

------------------------------------------------------------------------------
-- Ex 5: using the State monad, write the operation update that first
-- multiplies the state by 2 and then adds one to it. The state has
-- type Int.
--
-- Example:
--  runState update 3
--    ==> ((),7)

update :: State Int ()
update = do
  old <- get
  put $ (old * 2 ) + 1

------------------------------------------------------------------------------
-- Ex 6: using the State monad, walk through a list and add up all the
-- elements in the state. Additionally you should return the length of
-- the list.
--
-- Do this by implementing a recursive State operation lengthAndSum.
-- DO NOT use the functions length or filter.
--
-- Example:
--  runState (lengthAndSum [1,2,3,4]) 0
--    ==> (4,10)

lengthAndSum :: [Int] -> State Int Int
lengthAndSum [] = return 0
lengthAndSum (x:xs) = do
  l <- lengthAndSum xs
  modify (+x)
  return $ l+1

------------------------------------------------------------------------------
-- Ex 7: Let's use a state of type [a] to keep track of which elements
-- we've seen an odd number of times (1, 3, 5, ...)
--
-- Implement the State operation oddUpdate that updates the state,
-- given an element. In other words, if the element is in the state,
-- remove it, and if it is not in the state, add it.
--
-- Examples:
--   runState (oddUpdate 1) [] ==> ((),[1])
--   runState (oddUpdate 1) [1,2,3] ==> ((),[2,3])
--
-- PS. Order of the list in the state doesn't matter
-- note to self: hard1!

oddUpdate :: Eq a => a -> State [a] ()
oddUpdate x = modify (check x)
-- oddUpdate x = do check x
  where
    -- check :: Eq a => a -> [a] -> [a]
    -- check z [] = [z]
    -- check z [y] = if z == y then [] else [z,y]
    -- check z (y:ys) = if y == z then check z ys else check y ys
    
    check :: Eq a => a -> [a] -> [a]
    check z [] = [z]
    check z [y] = if z == y then [] else [z,y]
    check z (y:ys)
      | z == y    = ys
      | otherwise = y : check z ys

------------------------------------------------------------------------------
-- Ex 8: Define the operation oddsOp, so that the function odds
-- returns all the elements of a list that occur an odd number of
-- times.
--
-- Use the oddUpdate operation you just defined.
--
-- Examples:
--   odds [] ==> []
--   odds [1,2,3] ==> [1,2,3]
--   odds [1,1,2] ==> [2]
--
-- PS. Order of the returned list doesn't matter

odds :: Eq a => [a] -> [a]
odds xs = snd (runState (oddsOp xs) [])

oddsOp :: Eq a => [a] -> State [a] ()
oddsOp [] = return ()
oddsOp (x:xs) = do
  oddUpdate x
  oddsOp xs

------------------------------------------------------------------------------
-- Ex 9: implement the function ifM, that takes three monadic
-- operations. If the first of the operations returns True, the second
-- operation should be run. Otherwise the third operation should be
-- run.
--
-- Examples (test is defined below):
--  runState (put 11 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('b',11)
--  runState (put 9 >> ifM test (return 'a') (return 'b')) 0
--    ==> ('a',9)

test :: State Int Bool
test = do
  x <- get
  return (x<10)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  opBool >>= \x -> if x then opThen else opElse

------------------------------------------------------------------------------
-- Ex 10: the standard library function Control.Monad.mapM defines a
-- monadic map operation. Some examples of using it (safeDiv is defined
-- below):
--
-- mapM (safeDiv 10.0) [1.0,5.0,2.0]  =>  Just [10.0,2.0,5.0]
-- mapM (safeDiv 10.0) [1.0,0.0,2.0]  =>  Nothing
--
-- Your task is to implement the function mapM2 that works like mapM,
-- but there are two lists and the operation takes two arguments. If
-- the lists are of different lists, you can stop processing them once
-- the shorter one ends.
--
-- Examples:
--  mapM2 (\x y -> Just (x+y)) [1,2,3] [6,7]
--    ==> Just [7,9]
--  runState (mapM2 (\x y -> if x then modify (+y) else return () ) [True,False,True] [1,2,4]) 0
--    ==> ([(),(),()],5)

safeDiv :: Double -> Double -> Maybe Double
safeDiv x 0.0 = Nothing
safeDiv x y = Just (x/y)

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 op [] _ = return []
mapM2 op _ [] = return []
mapM2 op (x:xs) (y:ys) = do
  z <- op x y
  end <- mapM2 op xs ys
  return $ z:end

------------------------------------------------------------------------------
-- Ex 11&12: Funnykiztan has cities that are named with by integers
-- 0 .. n-1. Some cities are connected by roads. Your task is to find
-- out if you can can get from city A to city B by following the
-- roads.
--
-- The road network is given as an adjacency list, which means a list
-- of lists [[Int]] where the i'th list gives the cities to which city
-- i has a road to.
--
-- For example the road network:
--
-- 0--1
-- |\ |
-- | \|
-- 2--3
--
-- would be represented as:
--  [[1,2,3]
--  ,[0,3]
--  ,[0,3]
--  ,[0,1,2]]
--
-- Below you'll find the function routeExists that solves the task.
-- However a very important piece of the function, the helper function
-- dfs is still unimplemented.
--
-- The function dfs is intended to run a Depth-First Search. If you
-- don't know what this means, have a look in wikipedia.
--
-- Simply put, dfs uses roads to travel from city to city, using a
-- state of type [Int] to keep track of which cities have been
-- visited. This is important because the road network will have
-- cycles.
--
-- Examples:
--   routeExists example1 0 2  ==> True
--   routeExists example2 0 2  ==> True
--   routeExists example2 3 5  ==> False
--   runState (dfs example2 0) []  ==> ((),[2,3,1,0])
-- When 1 and 2 have already been visited, dfs won't proceed to city 3:
--   runState (dfs example1 0) [1,2] ==> ((),[0,1,2])
--
-- A word on tests. The tests first test the function dfs in a couple
-- of simple situations. After this they test the function routeExists
-- more extensively. The tests look at the state produced by dfs but
-- do not care in which order it is.

-- Three cities, each connected to the two others
example1 :: [[Int]]
example1 = [[1,2]
           ,[0,2]
           ,[0,1]]

-- A more two-part network:
--
-- 0 -- 1
-- |    |   4 -- 5
-- |    |
-- 2 -- 3
example2 :: [[Int]]
example2 = [[1,2]
           ,[0,3]
           ,[0,3]
           ,[1,2]
           ,[5]
           ,[4]]

routeExists :: [[Int]] -> Int -> Int -> Bool
routeExists cities i j = j `elem` execState (dfs cities i) []
-- Depth-first search
-- Wikipedia pseudocode: 
-- https://en.wikipedia.org/wiki/Depth-first_search#Pseudocode
-- 1  procedure DFS(G,v):
-- 2      label v as discovered
-- 3      for all edges from v to w in G.adjacentEdges(v) do
-- 4          if vertex w is not labeled as discovered then
-- 5              recursively call DFS(G,w)
-- note to self: quite hard, took longest
-- pseudo didnt help that much..

dfs :: [[Int]] -> Int -> State [Int] ()
dfs cities city = do
  -- visited cities from state
  discovered <- get
  let visited = city `elem` discovered
  if (not visited)
    then do
      remember
      rest
    else return ()
      where remember = modify (city:)
            rest = mapM_ (dfs cities) (cities !! city)
------------------------------------------------------------------------------
-- Ex 13: define the function orderedPairs that returns all pairs
-- (i,j) such that i<j and i occurs in the given list before j.
--
-- Use the list monad!
--
-- Examples:
--  orderedPairs [1,3,2,4]
--    ==> [(1,3),(1,2),(1,4),(3,4),(2,4)]
--
-- PS. once again the tests don't care about the order of results
-- Examples from material
-- http://pseudo.fixme.fi/~opqdonut/ifp2018/notes.html#one-more-monad
-- findSum :: [Int] -> Int -> [(Int,Int)]
-- findSum xs k = do a <- xs
--                   b <- xs
--                   if (a+b==k) then [(a,b)] else []
-- Or in other words:
-- findSum :: [Int] -> Int -> [(Int,Int)]
-- findSum xs k = do a <- xs
--                   b <- xs
--                   if (a+b==k) then return (a,b) else fail ""
-- findSum [1,2,3,4,5] 5
--   ==> [(1,4),(2,3),(3,2),(4,1)]
------------------------------
-- substrings :: String -> [String]
-- substrings xs = do i <- [0..length xs - 1]
--                    let maxlen = length xs - i
--                    j <- [1..maxlen]
--                    return $ take j $ drop i $ xs

-- palindromesIn :: String -> [String]
-- palindromesIn xs = do s <- substrings xs
--                       if (s==reverse s) then return s else fail ""

-- longestPalindrome xs = head . sortBy f $ palindromesIn xs
--   where f s s' = compare (length s') (length s)  -- longer is smaller
-- longestPalindrome "aabbacddcaca"
--   ==> "acddca"
------------------------------------------------------------

orderedPairs :: [Int] -> [(Int,Int)]
orderedPairs xs = do
  i <- is
  j <- is
  let x = xs !! i
  let y = xs !! j
  -- i<j and i occurs in the given list before j
  if i < j && x < y
    then return (x,y)
    else fail ""
  where
    is = [0..length xs - 1]

------------------------------------------------------------------------------
-- Ex 14: Using the list monad, produce a list of all pairs of
-- _different_ elements in a given list.
--
-- You can assume the list contains no duplicates.
--
-- DO NOT use map, list pattern matching. Use >>= or do notation.
--
-- Examples:
--   pairs [1] ==> []
--   pairs [1,2] ==> [(1,2),(2,1)]
--   pairs [1,2,3] ==> [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
--
-- PS. the order of the returned list does not matter
-- note : above with changed condition
pairs :: Eq a => [a] -> [(a,a)]
pairs xs = do
  i <- is
  j <- is
  let x = xs !! i
  let y = xs !! j
  if x /= y
    then return (x,y)
    else fail ""
  where
    is = [0..length xs - 1]

------------------------------------------------------------------------------
-- Ex 15: the standard library defines the function
--
--   foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
--
-- This function behaves like foldr, but the operation used is
-- monadic. foldM f acc xs works by running f for each element in xs,
-- giving it also the result of the previous invocation of f.
--
-- Your task is to implement the functions fsum so that the
-- function sumNotTwice works.
--
-- sumNotTwice computes the sum of a list, but ignores duplicated
-- elements. That is, only the first occurrence of a given number is
-- counted.
--
-- Examples:
--  sumNotTwice [1,1,1,1,1] ==> 1
--  sumNotTwice [5,-2,5]    ==> 3
--  sumNotTwice [1,2,-2,3]  ==> 4

sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM fsum 0 xs) []

fsum :: Int -> Int -> State [Int] Int
fsum acc x = do
  visited <- get
  if x `elem` visited then return acc else chain
  where
    chain = modify (x:) >> return (x+acc)

------------------------------------------------------------------------------
-- Ex 16: here is the Result type from last week. Implement a Monad
-- Result instance that behaves roughly like the Monad Maybe instance.
--
-- That is,
--   1. MkResults behave like Just
--   2. If part of computation produces NoResult, the whole computation
--      produces NoResult (just like Nothing)
--   3. Similarly, if we get a Failure "reason" value, the whole
--      computation produces Failure "reason"
--
-- Additionally, the method "fail" of the Monad type class should
-- produce a Failure.
--
-- Examples:
--   MkResult 1 >> Failure "boom" >> MkResult 2
--     ==> Failure "boom"
--   MkResult 1 >> NoResult >> Failure "not reached"
--     ==> NoResult
--   MkResult 1 >>= (\x -> MkResult (x+1))
--     ==> MkResult 2

data Result a = MkResult a | NoResult | Failure String deriving (Show,Eq)

-- A straightforward Functor instance
instance Functor Result where
  fmap f (MkResult a) = MkResult (f a)
  fmap _ NoResult = NoResult
  fmap _ (Failure s) = Failure s

-- Disregard this instance. In recent versions of the Haskell standard
-- library, all Monads must also be Applicative. These exercises don't
-- really cover Applicative.
instance Applicative Result where
  pure = return
  (<*>) = ap

-- Example from mateiral
-- instance  Monad Maybe  where
--   (Just x) >>= k      = k x
--   Nothing  >>= _      = Nothing

--   (Just _) >>  k      = k
--   Nothing  >>  _      = Nothing

--   return              = Just
--   fail _              = Nothing
instance Monad Result where
  -- implement return and >>=
    MkResult x >>= k  = k x
    NoResult   >>= _  = NoResult
    Failure x  >>= _  = Failure x

    return = MkResult
    fail s = Failure s
------------------------------------------------------------------------------
-- Ex 17&18: Here is the type SL that combines the State and Logger
-- types. Implement an instance Monad SL, that behaves like the
-- combination of State and Logger. That is, state is propagated from
-- one operation to the next, and log messages are stored in the order
-- they are produced.
--
-- To simplify the type signatures, the type of the state has been set
-- to Int, instead of being a parameter like in the standard State
-- monad.
--
-- This is a tough one. Keep trying and you'll get it!
--
-- You might find it easier to start with the Functor instance
--
-- Examples:
--   runSL (fmap (+1) getSL) 13
--      ==> (14,13,[])
--   runSL (fmap (const True) (msgSL "hello")) 17
--      ==> (True,17,["hello"])
--   runSL (putSL 2 >> msgSL "hello" >> getSL) 0
--      ==> (2,2,["hello"])
--   runSL (replicateM_ 5 (modifySL (+1) >> getSL >>= \x -> msgSL ("got "++show x))) 1
--      ==> ((),6,["got 2","got 3","got 4","got 5","got 6"])

data SL a = SL (Int -> (a,Int,[String]))

-- Run an SL operation with the given starting state
runSL :: SL a -> Int -> (a,Int,[String])
runSL (SL f) state = f state

-- Write a log message
msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((),s,[msg]))

-- Fetch the state
getSL :: SL Int
getSL = SL (\s -> (s,s,[]))

-- Overwrite the state
putSL :: Int -> SL ()
putSL s' = SL (\s -> ((),s',[]))

-- Modify the state
modifySL :: (Int->Int) -> SL ()
modifySL f = SL (\s -> ((),f s,[]))

instance Functor SL where
  -- implement fmap


-- again, disregard this
instance Applicative SL where
  pure = return
  (<*>) = ap

instance Monad SL where
  -- implement return and >>=
  -- Example from mateiral
  -- instance  Monad Maybe  where
  --   (Just x) >>= k      = k x
  --   Nothing  >>= _      = Nothing

  --   (Just _) >>  k      = k
  --   Nothing  >>  _      = Nothing

  --   return              = Just
  --   fail _              = Nothing
  
  -- x >>= k      = k x
