import Data.Foldable
import Numeric.Natural
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromMaybe)

maxSteps = Fin 100 -- Inf if you want to allow infinite loops

main = do
   print $ run adder [1,5,7] -- [0,0,13]
   print $ run subtractor [8,3] -- [0,0,5]
   print $ run subtractor [3,8] -- Bottom

adder = Start a where
   a = Jeqz 0 d b
   b = Dec 0 c
   c = Inc 2 a
   d = Jeqz 1 g e
   e = Dec 1 f
   f = Inc 2 d
   g = Halt

subtractor = Start a where
   a = Jeqz 1 e b
   b = Jeqz 0 b c
   c = Dec 0 d
   d = Dec 1 a
   e = Jeqz 0 h f
   f = Dec 0 g
   g = Inc 2 e
   h = Halt

-- Implementation

data CoNat = Fin !Natural | Inf deriving (Eq, Ord, Show)

prec :: CoNat -> Maybe CoNat
prec (Fin n) = if n == 0 then Nothing else Just $ Fin $ n-1
prec Inf = Just Inf

data Result = Bottom | Memory [Int]

instance Show Result where
   show Bottom = "Bottom"
   show (Memory x) = show x

data Machine = Start Op

data Op = Inc Int Op
        | Dec Int Op
        | Jeqz Int Op Op
        | Halt
   deriving Eq

(!?) :: IntMap Int -> Int -> Int
(!?) m k = M.findWithDefault 0 k m

toFullList :: IntMap Int -> [Int]
toFullList m = [ m !? k | k <- [0..maxKey] ]
   where maxKey = maybe (-1) fst $ M.lookupMax m

run :: Machine -> [Int] -> Result
run (Start op) memory = eval op maxSteps memoryMap
   where
      memoryMap = M.fromDistinctAscList $ zip [0..] memory

eval :: Op -> CoNat -> IntMap Int -> Result
eval op steps memory = 
   case prec steps of
      Nothing -> Bottom
      Just stepsLeft -> resume stepsLeft
   where
      resume s =
         case op of
            Inc register next ->
               eval next s $ M.alter safeInc register memory
            Dec register next ->
               eval next s $ M.alter safeDec register memory
            Jeqz register ifZero ifNonZero ->
               eval next s memory
                  where next = if isZero register then ifZero else ifNonZero
            Halt -> Memory $ toFullList memory
      safeInc = Just . (+ 1) . fromMaybe 0
      safeDec = Just . max 0 . subtract 1 . fromMaybe 0
      isZero register = memory !? register == 0
