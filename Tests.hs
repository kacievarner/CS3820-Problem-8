module Tests where

-- GHC
import System.Exit
import System.Environment
-- import System.IO.Silently

-- External
import Test.HUnit

import Data.Char (isSpace)
-- import Control.Monad
-- import Control.Monad.State hiding (get, put)

-- Lib
import Problem8

--------------------------------------------------------------------------------
-- util
--------------------------------------------------------------------------------

getVal :: M a -> Int -> a
getVal m z = a where
    (a, _, _) = run m z
    
state :: M a -> Int -> Int
state m z = mid where
  (_, mid, _) = run m z

m1 :: M Int
m1 = M (\n -> (n, n, [n]))

m2 :: a -> M a
m2 a = M (\n -> (a, 0, [0]))

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

next' :: Int -> Int
next' x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

collatz' :: Int -> [Int]
collatz' 1 = [1]
collatz' n = n : collatz' (next' n)

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

p1 :: Test
p1 = test [
  -- M functorality
  getVal (fmap (+1) m1) 1
    @?= 2,
  getVal (fmap show m1) 1
    @?= "1",
  getVal (fmap (flip replicate "foo") m1) 3
    @?= ["foo", "foo", "foo"],
  getVal (fmap read (m2 "1337")) 0
    @?= 1337,
  getVal (fmap read (m2 "10")) 0
    @?= getVal (fmap (*2) m1) 5,
  getVal (fmap Just m1) 5
    @?= Just 5,
  -- M functorality doesn't touchy the wrong bits
  trace (fmap (*2) m1) 10
    @?= [10],
  state (fmap (*2) m1) 10
    @?= 10,
  -- M is a monad
  getVal (return "foo") 3
    @?= "foo",
  state (return "foo") 3
    @?= 3,
  getVal (m1 >>= (\a -> return (a + 1))) 3
    @?= 4,
  trace (m1 >>= (\a -> return (a * 2))) 1 @?= [1],
  state (
      (m1 >>=
         (\a -> return (a * 2))) >>=
        (\b -> return (b + 2))) 1 @?= 1,
    getVal (
      (m1 >>=
        (\a -> return (a * 2))) >>=
        (\b -> return (b + 2))) 1 @?= 4
  ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------

p2 :: Test
p2 = test [
  -- get
  getVal (
      do
        x <- get
        return x
    ) 2 @?= 2,
    getVal (
      do
        x <- get
        return 100
      ) 1 @?= 100,
    trace (
      do
        x <- get
        y <- get
        return (x + y)
      ) 2 @?= [],
    state (
      do
        x <- get
        y <- get
        z <- get
        return x
      ) 10 @?= 10,
    -- put
    getVal (
      do
        put 20
        get
      ) 10 @?= 20,
    getVal (
      do
        put 1337
      ) 2 @?= (),
    state (
      do
        x <- get
        put x
        put (2 * x)
        put (3 * x)
      ) 2 @?= 6,
    state (
      do
        let inc = do
              x <- get
              put (x + 1)
        sequence_ (replicate 100 inc)
      ) 1 @?= 101,
    -- tell
    trace (
      do
        tell 2
          ) 1 @?= [2],
    trace (
      do
        x <- get
        tell (x + 1)
      ) 1 @?= [2],
    trace (
      do
        let incTell = do
              x <- get
              put (x + 1)
              tell x
        sequence_ (replicate 10 incTell)
      ) 1 @?= [1..10],
    getVal (
      do
        tell 5
      ) 1 @?= (),
    trace (
      let fibTell = do
            x <- get
            put (x + 1)
            tell (fib x)
      in sequence_ (replicate 10 fibTell)
      ) 1 @?= [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]    
  ]

p3 :: Test
p3 = test $ [
  -- next
  state next 100 @?= 50,
  trace next 11 @?= [11],
  getVal next 100 @?= ()
  ]
  ++ [state next x @?= next' x | x <- [42..55]]
  ++ [state collatz x @?= 1 | x <- [33..47]]
  ++ [trace collatz x @?= collatz' x | x <- [3..17]]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------



argMap :: Int -> Test
argMap 1 = p1
argMap 2 = p2
argMap _ = test [p1, p2, p3]

hd :: [a] -> Maybe a
hd (x : _) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let tests = case read <$> (hd args) of
                Just x -> argMap x
                Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
