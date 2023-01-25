module MyStuff where

import Control.Monad ( ap, liftM )
import MyStuff2

newtype D = D {runD :: Int} deriving (Show, Eq)
newtype D2 = D2 {runD2 :: Int -> Int}
newtype State s a = State { runState :: s -> (a, s) }
data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = State coin
pushS = State push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]



instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
    return x = State ( \ s -> (x, s) )
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    p >>= k = q where
        p' = runState p        -- p' :: s -> (a, s)
        k' = runState . k      -- k' :: a -> s -> (b, s)
        q' s0 = (y, s2) where  -- q' :: s -> (b, s)
            (x, s1) = p' s0    -- (x, s1) :: (a, s)
            (y, s2) = k' x s1  -- (y, s2) :: (b, s)
        q = State q'

data Tree a = Node a [Tree a] deriving (Show, Eq)
instance Functor Tree where
    fmap f  (Node n ts) =
        Node (f n) (map (fmap f) ts)




