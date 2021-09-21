-- Bulgarian Solitaire https://en.wikipedia.org/wiki/Bulgarian_solitaire
-- Take a pile of cards. Split them.
-- Take one card from each pile to make a new pile.
-- If any pile has no cards, delete it.
-- What is the sequence? Are there periodic cycles?
-- What card piles are impossible? How many piles will be made?

import Data.List ( elemIndex, sort )
import Data.Maybe ( fromJust )

data Run = Run {numCards   :: Int,
                stopAt     :: Int,
                repeatFrom :: Maybe Int,
                maxPiles   :: Int}
                deriving (Show)

-- number of cards to simulate
start :: Int
start = 3
stop :: Int
stop  = 500

cards :: [Int]
cards = [start..stop]

-- The split is to just take one card, but that can be adjusted
piles :: Int -> [[Int]]
piles x =  [[1..cut], [(cut + 1)..x]]
        where cut = 1

takeAcard :: [[Int]] -> [[Int]]
takeAcard xs = filter (not . null) $ map head xs : map tail xs

pileSize :: [[[Int]]] -> [[Int]]
pileSize = map (sort . map length)

hands :: Int -> [[Int]]
hands x = pileSize $ iterate takeAcard (piles x)

-- The fields returned are:
--   The the list of hands -> [[Int]]
--   Index where repetition happens -> Maybe Int
stopHands :: [[Int]] -> [[Int]] -> (Maybe Int,[[Int]])
stopHands seen (x:xs)
        | x `elem` seen = ((+1) <$> elemIndex x seen, [x])
        | otherwise = (fst s, x : snd s)
                where s = stopHands (seen ++ [x]) xs

runs :: [Run]
runs = [Run {numCards = x,
             stopAt = length (snd (s x)),
             repeatFrom = fst (s x),
             maxPiles = maximum (map length (snd (s x)))
             }| x <- cards]
        where s x = stopHands [] (hands x)

-- These all functions are to be used for adding a graph
-- But I have to figure out how to do plotting in Haskell
-- Call them with allxxxx runs
allCycleLen :: [Run] -> [[Int]]
allCycleLen rs = [[numCards r, stopAt r - fromJust (repeatFrom r)]| r <- rs]

allStops :: [Run] -> [[Int]]
allStops rs = [[numCards r, stopAt r] | r <- rs]

allMaxPiles :: [Run] -> [[Int]]
allMaxPiles rs = [[numCards r, maxPiles r] | r <- rs]

-- helper function for formatting output of runs
formatr :: Run -> [Char]
formatr (Run  {numCards = a, stopAt =b, repeatFrom = c, maxPiles = d}) =
        show a ++ " cards. " ++
        "Index " ++ show b ++
        " = index " ++ show (fromJust c) ++
        ". Cycle length " ++ show (b - fromJust c) ++
        ". Maximum piles " ++ show d ++ "."


main :: IO ()
main = do
        mapM_ print $ zip [1..] $ snd (stopHands [] (hands start))
        mapM_ (print . formatr) runs
        print "And now the card amounts that have cycle length 1:"
        mapM_ print $ filter (\[_,x] -> x == 1) $ allCycleLen runs


