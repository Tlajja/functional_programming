{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence input = case parseSimpleCommand input of
  Left err -> Left err
  Right (cmd, rest) -> 
    case parseWhitespace rest of
      Right (_, ';':rest2) -> 
        case parseWhitespace rest2 of
          Right (_, rest3) -> case parseSequence rest3 of
            Left err -> Left err
            Right (cmd2, rest4) -> Right (Lib1.Sequence cmd cmd2, rest4)
          Left err -> Left err
      Right (_, rest2) -> Right (cmd, rest2)
      Left err -> Left err

-- BNF: <simple-command> ::= <create-deck> | <shuffle-deck> | <draw-cards> | 
--                           <add-card> | <remove-card> | <show-deck> | 
--                           <count-cards> | <play-card> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand input = 
  case orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse
    parseCreateDeck
    parseShuffleDeck)
    parseDrawCards)
    parseAddCard)
    parseRemoveCard)
    parseShowDeck)
    parseCountCards)
    parsePlayCard)
    parseDump input of
      Right result -> Right result
      Left _ -> Left $ "Unknown command: '" ++ takeWhile (not . isWhitespaceChar) input ++ "'"


-- BNF: <create-deck> ::= "create" <whitespace> "deck"
parseCreateDeck :: Parser Lib1.Command
parseCreateDeck input = 
  and3 (parseString "create") parseWhitespace1 (parseString "deck") input
    >>= \((_, _), rest) -> Right (Lib1.CreateDeck, rest)

-- BNF: <shuffle-deck> ::= "shuffle" <whitespace> "deck"
parseShuffleDeck :: Parser Lib1.Command
parseShuffleDeck input =
  and3 (parseString "shuffle") parseWhitespace1 (parseString "deck") input
    >>= \((_, _), rest) -> Right (Lib1.ShuffleDeck, rest)

-- BNF: <draw-cards> ::= "draw" <whitespace> <number>
parseDrawCards :: Parser Lib1.Command
parseDrawCards input =
  and3 (parseString "draw") parseWhitespace1 parseNumber input
    >>= \((_, n), rest) -> Right (Lib1.DrawCards n, rest)

-- BNF: <add-card> ::= "add" <whitespace> <card>
parseAddCard :: Parser Lib1.Command
parseAddCard input =
  and3 (parseString "add") parseWhitespace1 parseCard input
    >>= \((_, card), rest) -> Right (Lib1.AddCard card, rest)

-- BNF: <remove-card> ::= "remove" <whitespace> <card>
parseRemoveCard :: Parser Lib1.Command
parseRemoveCard input =
  and3 (parseString "remove") parseWhitespace1 parseCard input
    >>= \((_, card), rest) -> Right (Lib1.RemoveCard card, rest)

-- BNF: <show-deck> ::= "show" <whitespace> "deck"
parseShowDeck :: Parser Lib1.Command
parseShowDeck input =
  and3 (parseString "show") parseWhitespace1 (parseString "deck") input
    >>= \((_, _), rest) -> Right (Lib1.ShowDeck, rest)

-- BNF: <count-cards> ::= "count" <whitespace> "cards"
parseCountCards :: Parser Lib1.Command
parseCountCards input =
  and3 (parseString "count") parseWhitespace1 (parseString "cards") input
    >>= \((_, _), rest) -> Right (Lib1.CountCards, rest)

-- BNF: <play-card> ::= "play" <whitespace> <card>
parsePlayCard :: Parser Lib1.Command
parsePlayCard input =
  and3 (parseString "play") parseWhitespace1 parseCard input
    >>= \((_, card), rest) -> Right (Lib1.PlayCard card, rest)

-- BNF: <dump> ::= "dump" <whitespace> "examples"
parseDump :: Parser Lib1.Command
parseDump input =
  and3 (parseString "dump") parseWhitespace1 (parseString "examples") input
    >>= \((_, _), rest) -> Right (Lib1.Dump Lib1.Examples, rest)

-- BNF: <card> ::= <rank> <whitespace> "of" <whitespace> <suit>
parseCard :: Parser Lib1.Card
parseCard input =
  and5 parseRank parseWhitespace1 (parseString "of") parseWhitespace1 parseSuit input
    >>= \(((((r, _), _), _), s), rest) -> Right (Lib1.Card { Lib1.suit = s, Lib1.rank = r }, rest)

-- BNF: <rank> ::= "Ace" | "Two" | "Three" | "Four" | "Five" | "Six" | "Seven" | 
--                 "Eight" | "Nine" | "Ten" | "Jack" | "Queen" | "King"
parseRank :: Parser Lib1.Rank
parseRank = orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse (orElse
  (parseString "Ace" `withResult` Lib1.Ace)
  (parseString "Two" `withResult` Lib1.Two))
  (parseString "Three" `withResult` Lib1.Three))
  (parseString "Four" `withResult` Lib1.Four))
  (parseString "Five" `withResult` Lib1.Five))
  (parseString "Six" `withResult` Lib1.Six))
  (parseString "Seven" `withResult` Lib1.Seven))
  (parseString "Eight" `withResult` Lib1.Eight))
  (parseString "Nine" `withResult` Lib1.Nine))
  (parseString "Ten" `withResult` Lib1.Ten))
  (parseString "Jack" `withResult` Lib1.Jack))
  (parseString "Queen" `withResult` Lib1.Queen))
  (parseString "King" `withResult` Lib1.King)

-- BNF: <suit> ::= "Hearts" | "Clubs" | "Spades" | "Diamonds"
parseSuit :: Parser Lib1.Suit
parseSuit = orElse (orElse (orElse
  (parseString "Hearts" `withResult` Lib1.Hearts)
  (parseString "Clubs" `withResult` Lib1.Clubs))
  (parseString "Spades" `withResult` Lib1.Spades))
  (parseString "Diamonds" `withResult` Lib1.Diamonds)

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber [] = Left "Expected number"
parseNumber input = 
  case span isDigitChar input of
    ([], _) -> Left "Expected number"
    (digits, rest) -> Right (read digits, rest)

-- BNF: <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseDigit :: Parser Char
parseDigit [] = Left "Expected digit"
parseDigit (c:rest)
  | isDigitChar c = Right (c, rest)
  | otherwise = Left "Expected digit"

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

-- BNF: <string> ::= <char> | <char> <string>
parseString :: String -> Parser String
parseString expected input
  | take (length expected) input == expected = Right (expected, drop (length expected) input)
  | otherwise = Left $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace input = 
  let (ws, rest) = span isWhitespaceChar input
  in Right (ws, rest)

-- Parse at least one whitespace character
parseWhitespace1 :: Parser String
parseWhitespace1 [] = Left "Expected whitespace"
parseWhitespace1 (c:rest)
  | isWhitespaceChar c = 
      let (ws, rest2) = span isWhitespaceChar rest
      in Right (c:ws, rest2)
  | otherwise = Left "Expected whitespace"

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- Parser combinators
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right result -> Right result
  Left _ -> p2 input

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = case p1 input of
  Left err -> Left err
  Right (r1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (r2, rest2) -> Right ((r1, r2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser ((a, b), c)
and3 p1 p2 p3 = and2 (and2 p1 p2) p3

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (((a, b), c), d)
and4 p1 p2 p3 p4 = and2 (and3 p1 p2 p3) p4

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser ((((a, b), c), d), e)
and5 p1 p2 p3 p4 p5 = and2 (and4 p1 p2 p3 p4) p5

withResult :: Parser a -> b -> Parser b
withResult p result input = case p input of
  Left err -> Left err
  Right (_, rest) -> Right (result, rest)

-- Process function
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

-- ToCliCommand instance
class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand Lib1.CreateDeck = "create deck"
  toCliCommand Lib1.ShuffleDeck = "shuffle deck"
  toCliCommand (Lib1.DrawCards n) = "draw " ++ show n
  toCliCommand (Lib1.AddCard card) = "add " ++ cardToString card
  toCliCommand (Lib1.RemoveCard card) = "remove " ++ cardToString card
  toCliCommand Lib1.ShowDeck = "show deck"
  toCliCommand Lib1.CountCards = "count cards"
  toCliCommand (Lib1.PlayCard card) = "play " ++ cardToString card
  toCliCommand (Lib1.Sequence cmd1 cmd2) = toCliCommand cmd1 ++ "; " ++ toCliCommand cmd2
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

cardToString :: Lib1.Card -> String
cardToString (Lib1.Card s r) = rankToString r ++ " of " ++ suitToString s

rankToString :: Lib1.Rank -> String
rankToString Lib1.Ace = "Ace"
rankToString Lib1.Two = "Two"
rankToString Lib1.Three = "Three"
rankToString Lib1.Four = "Four"
rankToString Lib1.Five = "Five"
rankToString Lib1.Six = "Six"
rankToString Lib1.Seven = "Seven"
rankToString Lib1.Eight = "Eight"
rankToString Lib1.Nine = "Nine"
rankToString Lib1.Ten = "Ten"
rankToString Lib1.Jack = "Jack"
rankToString Lib1.Queen = "Queen"
rankToString Lib1.King = "King"

suitToString :: Lib1.Suit -> String
suitToString Lib1.Hearts = "Hearts"
suitToString Lib1.Clubs = "Clubs"
suitToString Lib1.Spades = "Spades"
suitToString Lib1.Diamonds = "Diamonds"

-- Eq instance for Command
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.CreateDeck == Lib1.CreateDeck = True
  Lib1.ShuffleDeck == Lib1.ShuffleDeck = True
  (Lib1.DrawCards n1) == (Lib1.DrawCards n2) = n1 == n2
  (Lib1.AddCard c1) == (Lib1.AddCard c2) = c1 == c2
  (Lib1.RemoveCard c1) == (Lib1.RemoveCard c2) = c1 == c2
  Lib1.ShowDeck == Lib1.ShowDeck = True
  Lib1.CountCards == Lib1.CountCards = True
  (Lib1.PlayCard c1) == (Lib1.PlayCard c2) = c1 == c2
  (Lib1.Sequence cmd1a cmd1b) == (Lib1.Sequence cmd2a cmd2b) = cmd1a == cmd2a && cmd1b == cmd2b
  (Lib1.Dump d1) == (Lib1.Dump d2) = d1 == d2
  _ == _ = False

-- Need Eq instances for Card, Rank, Suit, Dumpable
instance Eq Lib1.Card where
  (Lib1.Card s1 r1) == (Lib1.Card s2 r2) = s1 == s2 && r1 == r2

instance Eq Lib1.Dumpable where
  Lib1.Examples == Lib1.Examples = True
