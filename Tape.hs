module Tape (Tape, newtape, getvalue, TapeOp(..), operate) where

-------------------------
-- Tape data structure --
-------------------------

data InfList a = Cons a (InfList a)
rep :: a -> InfList a
rep x = Cons x (rep x)

data Tape = Head (InfList Int) Int (InfList Int)

newtape :: Tape
newtape = Head (rep 0) 0 (rep 0)

getvalue :: Tape -> Int
getvalue (Head _ a _) = a

setvalue :: Int -> Tape -> Tape
setvalue a (Head ls _ rs) = Head ls a rs

tapeLeft  :: Tape -> Tape
tapeLeft  (Head (Cons l ls) a rs) = Head ls l (Cons a rs)

tapeRight :: Tape -> Tape
tapeRight (Head ls a (Cons r rs)) = Head (Cons a ls) r rs

tapeInc :: Tape -> Tape
tapeInc (Head ls a rs) = Head ls (mod (succ a) 256) rs

tapeDec :: Tape -> Tape
tapeDec (Head ls a rs) = Head ls (mod (pred a) 256) rs

tapeIn :: Tape -> IO Tape
tapeIn t = getChar >>= \c -> putStrLn "" >> (return $ setvalue (fromEnum c) t)

tapeOut :: Tape -> IO Tape
tapeOut t = (putChar $ toEnum $ getvalue t) >> return t

----------------------------------
-- Primitive operations on Tape --
----------------------------------

data TapeOp = L | R | Inc | Dec | In | Out
  deriving (Eq, Show)

operate :: TapeOp -> Tape -> IO Tape
operate L   = return . tapeLeft
operate R   = return . tapeRight
operate Inc = return . tapeInc
operate Dec = return . tapeDec
operate In  = tapeIn
operate Out = tapeOut
