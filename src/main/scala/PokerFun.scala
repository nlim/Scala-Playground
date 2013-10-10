import scalaz._
import Scalaz._
import effect.IO

object PokerFun {

  sealed trait Rank
  case object Ace extends Rank
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank

  sealed trait Suit
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  case class Card(suit: Suit, rank: Rank)

  case class Hand(a: Card, b: Card, c: Card, d: Card, e: Card)

  sealed trait HandType
  case class StraightFlush(highRank: Rank) extends HandType
  case class Quads(qRank: Rank, unpaired: List[Rank]) extends HandType
  case class FullHouse(tripRank: Rank, pairRank: Rank) extends HandType
  case class Flush(cards: List[Rank]) extends HandType
  case class Straight(highRank: Rank) extends HandType
  case class Trips(tripRank: Rank, unpaired: List[Rank]) extends HandType
  case class TwoPair(highPair: Rank, lowPair: Rank, unpaired: List[Rank]) extends HandType
  case class Pair(pairRank: Rank, unpaired: List[Rank]) extends HandType
  case class NoPair(unpaired: List[Rank]) extends HandType

  val partialMakeRank: PartialFunction[Int, Rank] =
    {
      case 1 => Ace
      case 2 => Two
      case 3 => Three
      case 4 => Four
      case 5 => Five
      case 6 => Six
      case 7 => Seven
      case 8 => Eight
      case 9 => Nine
      case 10 => Ten
      case 11 => Jack
      case 12 => Queen
      case 13 => King
    }

  val makeRank: Int => Option[Rank] = partialMakeRank.lift

  val partialMakeSuit: PartialFunction[String, Suit] =
    {
      case "C" => Clubs
      case "H" => Hearts
      case "D" => Diamonds
      case "S" => Spades
    }

  val makeSuit: String => Option[Suit] = partialMakeSuit.lift

  //val makeCard = (Card(_, _))

  def main: IO[Unit] =
    for {
      input <- IO.readLn
      _ <- IO.putStrLn(input)
    } yield ()


}





/*
import Data.List.Split
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List

--- Data Types
--- newtype Rank = Rank { getRank :: Int } deriving (Show, Ord, Eq)

data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Show, Eq)
--- data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show, Eq, Ord)

data Card = Card Suit Rank deriving (Show)

data Hand = Hand Card Card Card Card Card deriving (Show)

--- Representation of PokerHands to encourage easy comparison
data HandType = StraightFlush Rank | Quads Rank [Rank] | FullHouse Rank Rank | Flush [Rank] | Straight Rank | Trips Rank [Rank] | TwoPair Rank Rank [Rank] | Pair Rank [Rank] | NoPair [Rank] deriving (Show)

data Try a = Failure String | Success a deriving (Show)

instance Functor Try where
  fmap f (Success a) = Success (f a)
  fmap f (Failure s) = Failure s

instance Monad Try where
  (Success a) >>= f = f a
  (Failure s) >>= f = Failure s
  return a = Success a;

instance Eq Card where
  (==) (Card s1 r1) (Card s2 r2) = (==) r1 r2

instance Ord Card where
  compare (Card s1 r1) (Card s2 r2) = compare r1 r2

instance Ord Rank where
  compare r1 r2 = compare (value r1) (value r2)


instance Eq HandType where
  (StraightFlush r1) == (StraightFlush r2) = (==) r1 r2
  (Quads r1 (rh1:[])) ==  (Quads r2 (rh2:[])) = (==) r1 r2
  (FullHouse r11 r12) == (FullHouse r21 r22) = ((==) r11 r21) && ((==) r21 r22)
  (Flush rs1) == (Flush rs2) = rs1 == rs2
  (Straight r1) == (Straight r2) = r1 == r2
  (Trips r1 rs1) == (Trips r2 rs2) = (r1 == r2) && (rs1 == rs2)
  (TwoPair p11 p12 rs1) == (TwoPair p21 p22 rs2) = (p11 == p21) && (p12 == p22) && (rs1 == rs2)
  (Pair p1 rs1) == (Pair p2 rs2) = (p1 == p2) && (rs1 == rs2)
  (NoPair rs1) == (NoPair rs2) = rs1 == rs2
  _ == _ = False

--- Requires that the lists are in descending sorted order
instance Ord HandType where
  compare (StraightFlush r1) (StraightFlush r2) = compare r1 r2
  compare (Quads r1 (rh1:[])) (Quads r2 (rh2:[])) = (compare r1 r2) `ifEqThen` (compare rh1 rh2)
  compare (FullHouse r11 r12) (FullHouse r21 r22) = (compare r11 r21) `ifEqThen` (compare r12 r22)
  compare (Flush rs1) (Flush rs2) = (compare rs1 rs2)
  compare (Straight r1)  (Straight r2) = (compare r1 r2)
  compare (Trips r1 rs1) (Trips r2 rs2) = (compare r1 r2) `ifEqThen` (compare rs1 rs2)
  compare (TwoPair p11 p12 rs1) (TwoPair p21 p22 rs2) =  (compare p11 p21) `ifEqThen` (compare p12 p22) `ifEqThen` (compare rs1 rs2)
  compare (Pair p1 rs1) (Pair p2 rs2) = (compare p1 p2) `ifEqThen` (compare rs1 rs2)
  compare (NoPair rs1) (NoPair rs2) = compare rs1 rs2
  compare h1 h2 = compare (value h1) (value h2)

ifEqThen :: Ordering -> Ordering -> Ordering
ifEqThen EQ y = y
ifEqThen x _ = x

instance Eq Hand where
  (==) h1 h2 = (==) (getHandType h1) (getHandType h2)

instance Ord Hand where
  compare h1 h2 = compare (getHandType h1) (getHandType h2)

class Valuable a where
  value :: a -> Int

instance Valuable HandType where
  value (StraightFlush _) = 9
  value (Quads _ _) = 8
  value (FullHouse _ _) = 7
  value (Flush _) = 6
  value (Straight _) = 5
  value (Trips _ _) = 4
  value (TwoPair _ _ _) = 3
  value (Pair _ _) = 2
  value ht = 1 -- NoPair

instance Valuable Rank where
  value Ace = 14
  value King = 13
  value Queen = 12
  value Jack = 11
  value Ten = 10
  value Nine = 9
  value Eight = 8
  value Seven = 7
  value Six = 6
  value Five = 5
  value Four = 4
  value Three = 3
  value Two = 2


--- Creation Functions
makeRank :: Int -> Try Rank
makeRank 1 = Success Ace
makeRank 2 = Success Two
makeRank 3 = Success Three
makeRank 4 = Success Four
makeRank 5 = Success Five
makeRank 6 = Success Six
makeRank 7 = Success Seven
makeRank 8 = Success Eight
makeRank 9 = Success Nine
makeRank 10 = Success Ten
makeRank 11 = Success Jack
makeRank 12 = Success Queen
makeRank 13 = Success King
makeRank n = Failure ("Rank integer: " ++ (show n) ++ " is not between 1..13")


makeSuit :: Char -> Try Suit
makeSuit 'C' =  Success Clubs
makeSuit 'D' = Success Diamonds
makeSuit 'H' = Success Hearts
makeSuit 'S' = Success Spades
makeSuit c = Failure ("Suit Char: " ++ [c] ++ " is not C or D or H or S")

makeCard :: String -> Try Card
makeCard s = do
  (suitChar, rankS) <- parsePieces s
  suit <- makeSuit suitChar
  rank <- makeRank $ parseInt rankS
  return (Card suit rank)

pokerHand :: [Card] -> Try Hand
pokerHand (a:b:c:d:e:[]) = Success (Hand a b c d e)
pokerHand list = Failure ("List of Cards: " ++ (show list) ++ " is not 5 cards long")

--- Parsing Functions
parsePieces :: String -> Try (Char, String)
parsePieces s =
    case splitAt 1 s of
      (h:[], j) -> Success (h, j)
      otherwise -> Failure ("String: " ++ s ++ " could not be parse into pieces")

parseInt :: String -> Int
parseInt = read

parseHand :: String -> Try Hand
parseHand s = do
  cards <- mapM makeCard (splitOn " " s)
  hand <- pokerHand cards
  return hand

--- Computation Functions

cardList :: Hand -> [Card]
cardList (Hand c1 c2 c3 c4 c5) = [c1, c2, c3, c4, c5]

rankList :: Hand -> [Rank]
rankList h = map (\c -> case c of Card _ r -> r) (cardList h)

suitList :: Hand -> [Suit]
suitList h = map (\c -> case c of Card s _ -> s) (cardList h)

incrementHistogram :: (Ord a) => a -> (Map.Map a Int) -> (Map.Map a Int)
incrementHistogram k m = case Map.lookup k m of
                           Just v -> Map.insert k (v + 1) m
                           Nothing -> Map.insert k 1 m

populateHistogram :: (Ord a) => [a] -> (Map.Map a Int)
populateHistogram list = foldr incrementHistogram Map.empty list

getRankHistogram :: Hand -> Map.Map Rank Int
getRankHistogram h = populateHistogram $ rankList h

getSuitHistogram :: Hand -> Map.Map Suit Int
getSuitHistogram h = populateHistogram $ suitList h

--- foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
invertedRankHistogram :: Hand -> (Map.Map Int [Rank])
invertedRankHistogram h = Map.foldWithKey collectRankByCount (Map.empty) (getRankHistogram h)
---collectRankByCount :: Rank -> Int -> (Map.Map Int [Rank]) -> (Map.Map Int [Rank])
collectRankByCount :: Rank -> Int -> (Map.Map Int [Rank]) -> (Map.Map Int [Rank])
collectRankByCount r count m = case (Map.lookup count m) of
                                 Just rs -> Map.insert count (r:rs) m
                                 Nothing -> Map.insert count [r] m

isFlush :: Hand -> Bool
isFlush h = (Map.size (getSuitHistogram h)) == 1

isWheel :: [Rank]-> Bool
isWheel rs = case (List.sort rs) of
               [Two,Three,Four,Five,Ace] -> True
               otherwise -> False

isStraight :: Hand -> Bool
isStraight h = (isWheel sortedRanks) || all isAdj (selfZip sortedRanks)
               where sortedRanks = (List.sort . rankList) h
                     isAdj (r1,r2) = (value r1 + 1) == (value r2)

selfZip :: [a] -> [(a,a)]
selfZip as = zip as (tail as)

getHandType :: Hand -> HandType
getHandType h = case (flush, straight, quads, trips, pairs, singles) of
                  (True, True, _, _, _, ss) -> if isWheel ss then StraightFlush Five else StraightFlush (head ss)
                  (False, False, (q:[]), _, _, (s:t)) -> Quads q singles
                  (False, False, _, (t:[]), (p:[]), []) -> FullHouse t p
                  (True, False, _, _, _, _) -> Flush singles
                  (False, True, _, _, _, ss) -> if isWheel ss then Straight Five else Straight (head ss)
                  (False, False, _, (t:[]), [], (s1:s2:[])) -> Trips t singles
                  (False, False, _, _, (p1:p2:[]), (s:[])) -> TwoPair p1 p2 singles
                  otherwise -> NoPair singles
                  where  flush = isFlush h
                         straight = isStraight h
                         ranksByFreq = invertedRankHistogram h
                         descRanksOfFreq freq = maybe [] (reverse . List.sort) (Map.lookup freq ranksByFreq)
                         singles = descRanksOfFreq 1
                         pairs = descRanksOfFreq 2
                         trips = descRanksOfFreq 3
                         quads = descRanksOfFreq 4






--- Main
main :: IO ()
main = forever $ do
  putStrLn "Input Hand: e.g. C1 C2 H3 C4 D5"
  line <- getLine
  putStrLn (show (fmap getHandType (parseHand line)))
*/
