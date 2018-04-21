import Data.Sequence as DS
import Data.Foldable
import Numeric.Natural

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

growingUpdate :: Int -> Int -> Seq Int -> Seq Int
growingUpdate i val x =
   if DS.length x - 1 < i
      then growingUpdate i val (x |> 0)
      else update i val x

-- growing index
(!?) :: Seq Int -> Int -> Int
(!?) x i =
   if DS.length x - 1 < i
      then (x |> 0) !? i
      else index x i

run :: Machine -> [Int] -> Result
run (Start op) memory = eval op maxSteps $ fromList memory

eval :: Op -> CoNat -> Seq Int -> Result
eval op steps memory = 
   case prec steps of
      Nothing -> if op == Halt then halt else Bottom
      Just stepsLeft -> resume stepsLeft
   where
      resume s =
         case op of
            Inc register next ->
               eval next s $ growingUpdate register val memory
                  where val = memory !? register + 1
            Dec register next ->
               eval next s $ growingUpdate register val memory
                  where val = max 0 $ memory !? register - 1
            Jeqz register ifZero ifNonZero ->
               eval next s memory
                  where next = if memory !? register == 0 then ifZero else ifNonZero
            Halt -> halt
      halt = Memory $ toList memory
