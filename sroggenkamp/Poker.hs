-- Poker.hs - evaluate 5 card poker hands
--
-- Steve Roggenkamp
import Data.Char
import Data.Eq
import Data.Ord
import Data.List
import Data.Tuple

main = do
  evaluateHands

evaluateHands = do
  evaluateHand
  evaluateHands
  
evaluateHand = do
  line    <- getLine
  let p1      = span (/= ':') line
      player1 = fst p1
      rest1   = span (\c -> c == ':' || isSpace(c)) (snd p1)
      hand1   = parseHand 5 ( snd rest1)
      value1  = scoreHand (fst hand1)
      ws      = span isSpace (snd hand1)
      p2      = span (/= ':') (snd ws)
      player2 = fst p2
      rest2   = span (\c -> c == ':' || isSpace(c)) (snd p2)
      hand2   = parseHand 5 ( snd rest2)
      value2  = scoreHand (fst hand2)
      rank    = rankHand (fst hand2)
      winner  = case (value1,value2) of
       _ | value1 == value2  -> "Tie"
         | value1 >  value2  -> player1
         | value1 <  value2  -> player2
      in putStrLn( line ++ "  winner:  "++ winner
                   ++"  with "++ show(rank) )

parseHand :: Int -> String -> ([Card], String)
parseHand 0 s  = ([], s)
parseHand n str =
  let c1 = span isSpace str
      c2 = span (\c -> c `elem` ['2','3','4','5','6','7','8','9',
                                 'A','C','D','H','J','K','Q','S','T']) (snd c1)
      rest = parseHand (n-1) (snd c2)
  in ((read( fst c2):fst rest), snd rest)
   

data CardValue = Two   | Three | Four  | Five | Six   | Seven |
                 Eight | Nine  | Ten   | Jack | Queen | King  | Ace
               deriving (Bounded, Eq, Ord, Enum, Show)

instance Read CardValue where
  readsPrec _ val =
    let parseTbl = zip "23456789TJQKA" [Two .. Ace]
    in case lookup (head val) parseTbl of
      Just    v -> [(v, tail val)]
      Nothing   -> error $ "Invalid card value: " ++ val

data CardSuit  = Clubs | Diamonds | Hearts | Spades
               deriving (Bounded, Eq, Ord, Enum, Show)
                        
instance Read CardSuit where
  readsPrec _ val =
    let parseTbl = zip "CDHS" [Clubs .. Spades]
    in case lookup (head val) parseTbl of
      Just    s -> [(s, tail val)]
      Nothing   -> error $ "Invalid card suit: " ++ val

data Card = Card { val::CardValue, suit::CardSuit}
          deriving (Bounded, Eq, Show )

instance Read Card where
  readsPrec _ [r, s] = [(Card (read [r]) (read [s]), "")]
  readsPrec _ value = error $ "Invalid card: " ++ value
                        
instance Ord Card where
  compare c0 c1 = val(c0) `compare` val(c1)
                          
type Hand      = [Card]

data HandRank = HighCard | Pair      | TwoPairs   | ThreeOfKind | Straight
              | Flush    | FullHouse | FourOfKind | StraightFlush
              deriving (Bounded, Eq, Ord, Enum, Read, Show)

highCard :: Hand -> Card
highCard = maximum

isFlush :: Hand -> Bool
isFlush = (1 == ) . length . group . map suit

isStraight :: Hand -> Bool
isStraight hand =
  let cardvals     = sort $ map val hand
      straightvals = [(head cardvals) .. (last cardvals)]
  in cardvals == straightvals

nSame :: Int -> [[a]] -> Bool
nSame n groups = n `elem` map length groups

twoOfKind   :: [[a]] -> Bool 
twoOfKind   = nSame 2

threeOfKind   :: [[a]] -> Bool 
threeOfKind = nSame 3

fourOfKind   :: [[a]] -> Bool 
fourOfKind  = nSame 4

twoPairs :: [[a]] -> Bool
twoPairs groups = length( filter (2 == ) (map length groups)) == 2
  
scoreHand :: Hand -> Int
scoreHand hand =
  let maxVal = fromEnum(Ace)
  in
  fromEnum(rankHand hand)*fromEnum(StraightFlush)*maxVal*maxVal*maxVal*maxVal*maxVal+
  foldr (\c v -> v*maxVal+fromEnum(c)) 0 (sort $ map val hand)

rankHand :: Hand -> HandRank
rankHand hand =
  let cardValues  = map val hand
      valueGroups = sortByLength $ group cardValues
      ranks = map (!!0) valueGroups
      handRank hr = case hand of
        _  | isFlush hand && isStraight hand                  -> StraightFlush
           | fourOfKind valueGroups                           -> FourOfKind
           | threeOfKind valueGroups && twoOfKind valueGroups -> FullHouse
           | isFlush hand                                     -> Flush
           | isStraight hand                                  -> Straight
           | threeOfKind valueGroups                          -> ThreeOfKind
           | twoPairs  valueGroups                            -> TwoPairs 
           | twoOfKind valueGroups                            -> Pair
           | otherwise                                        -> HighCard
  in handRank ranks
     
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (flip $ comparing length)
