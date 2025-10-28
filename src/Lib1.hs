module Lib1
    ( examples, Command(..), Dumpable(..)
    , Card(..), Rank (..), Suit(..)
    ) where


data Card = Card
  { suit :: Suit
  , rank :: Rank
  } deriving Show


data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Show, Eq, Ord)

data Suit = Hearts | Clubs | Spades | Diamonds
  deriving (Show, Eq, Ord)

data Deck = Deck 
    {
    deckCards :: [Card]
    } deriving Show

data Hand = Hand
    {
    handCards :: [Card] 
    } deriving Show

data Command
    = CreateDeck
    | ShuffleDeck
    | DrawCards Int
    | AddCard Card
    | RemoveCard Card
    | ShowDeck
    | CountCards
    | PlayCard Card
    | Sequence Command Command
    | Dump Dumpable
    deriving Show


data Dumpable = Examples
  deriving Show

aceOfSpades :: Card
aceOfSpades = Card { suit = Spades, rank = Ace }

kingOfHearts :: Card  
kingOfHearts = Card { suit = Hearts, rank = King }

queenOfDiamonds :: Card
queenOfDiamonds = Card { suit = Diamonds, rank = Queen }

myDeck :: Deck
myDeck = Deck { deckCards = [aceOfSpades, kingOfHearts, queenOfDiamonds]}

fullHouseDeck :: Deck
fullHouseDeck = Deck
  { deckCards =
      [ Card { suit = Spades, rank = Ace }  
      , Card { suit = Hearts, rank = Ace }
      , Card { suit = Diamonds, rank = Ace }
      , Card { suit = Clubs, rank = King }
      , Card { suit = Spades, rank = King }
      ]
  }

example1 :: Command
example1 = CreateDeck

example2 :: Command  
example2 = ShuffleDeck

example3 :: Command
example3 = DrawCards 5

example4 :: Command
example4 = Sequence CreateDeck (Sequence ShuffleDeck (DrawCards 3))


examples :: [Command]
examples = [
    example1,
    example2,
    example3,
    example4,
    Dump Examples
    ]
