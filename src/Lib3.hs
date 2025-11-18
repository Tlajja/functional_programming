{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1
import Control.Applicative (Alternative(..))
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Concurrent (Chan, readChan, newChan, writeChan)
import System.Random (randomRIO)
import qualified System.IO.Strict as Strict
import System.IO (writeFile)
import Control.Exception (catch)
import System.IO.Error (IOError)
import Prelude hiding (writeFile)

-- ============================================================================
-- Parser Definition with Functor, Applicative, Alternative
-- ============================================================================

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Right (x, input)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser pa) = Parser $ \input -> case pf input of
        Left err -> Left err
        Right (f, rest1) -> case pa rest1 of
            Left err -> Left err
            Right (a, rest2) -> Right (f a, rest2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left "No parse"
    
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
        Right result -> Right result
        Left _ -> p2 input

-- ============================================================================
-- Basic Parser Combinators
-- ============================================================================

-- BNF: <string> ::= <char> | <char> <string>
parseString :: String -> Parser String
parseString expected = Parser $ \input ->
    if take (length expected) input == expected
    then Right (expected, drop (length expected) input)
    else Left $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace = Parser $ \input ->
    let (ws, rest) = span isWhitespaceChar input
    in Right (ws, rest)

parseWhitespace1 :: Parser String
parseWhitespace1 = Parser $ \input -> case input of
    [] -> Left "Expected whitespace"
    (c:rest) -> if isWhitespaceChar c
        then let (ws, rest2) = span isWhitespaceChar rest
             in Right (c:ws, rest2)
        else Left "Expected whitespace"

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber = Parser $ \input -> case input of
    [] -> Left "Expected number"
    _ -> case span isDigitChar input of
        ([], _) -> Left "Expected number"
        (digits, rest) -> Right (read digits, rest)

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

-- ============================================================================
-- Card Parsing
-- ============================================================================

-- BNF: <rank> ::= "Ace" | "Two" | "Three" | "Four" | "Five" | "Six" | "Seven" | 
--                 "Eight" | "Nine" | "Ten" | "Jack" | "Queen" | "King"
parseRank :: Parser Lib1.Rank
parseRank = 
    (parseString "Ace" *> pure Lib1.Ace) <|>
    (parseString "Two" *> pure Lib1.Two) <|>
    (parseString "Three" *> pure Lib1.Three) <|>
    (parseString "Four" *> pure Lib1.Four) <|>
    (parseString "Five" *> pure Lib1.Five) <|>
    (parseString "Six" *> pure Lib1.Six) <|>
    (parseString "Seven" *> pure Lib1.Seven) <|>
    (parseString "Eight" *> pure Lib1.Eight) <|>
    (parseString "Nine" *> pure Lib1.Nine) <|>
    (parseString "Ten" *> pure Lib1.Ten) <|>
    (parseString "Jack" *> pure Lib1.Jack) <|>
    (parseString "Queen" *> pure Lib1.Queen) <|>
    (parseString "King" *> pure Lib1.King)

-- BNF: <suit> ::= "Hearts" | "Clubs" | "Spades" | "Diamonds"
parseSuit :: Parser Lib1.Suit
parseSuit = 
    (parseString "Hearts" *> pure Lib1.Hearts) <|>
    (parseString "Clubs" *> pure Lib1.Clubs) <|>
    (parseString "Spades" *> pure Lib1.Spades) <|>
    (parseString "Diamonds" *> pure Lib1.Diamonds)

-- BNF: <card> ::= <rank> <whitespace> "of" <whitespace> <suit>
parseCard :: Parser Lib1.Card
parseCard = flip Lib1.Card 
    <$> (parseRank <* parseWhitespace1 <* parseString "of" <* parseWhitespace1)
    <*> parseSuit

-- ============================================================================
-- Command Parsing
-- ============================================================================

-- BNF: <create-deck> ::= "create" <whitespace> "deck"
parseCreateDeck :: Parser Lib1.Command
parseCreateDeck = parseString "create" *> parseWhitespace1 *> parseString "deck" *> pure Lib1.CreateDeck

-- BNF: <shuffle-deck> ::= "shuffle" <whitespace> "deck"
parseShuffleDeck :: Parser Lib1.Command
parseShuffleDeck = parseString "shuffle" *> parseWhitespace1 *> parseString "deck" *> pure Lib1.ShuffleDeck

-- BNF: <draw-cards> ::= "draw" <whitespace> <number>
parseDrawCards :: Parser Lib1.Command
parseDrawCards = Lib1.DrawCards <$> (parseString "draw" *> parseWhitespace1 *> parseNumber)

-- BNF: <add-card> ::= "add" <whitespace> <card>
parseAddCard :: Parser Lib1.Command
parseAddCard = Lib1.AddCard <$> (parseString "add" *> parseWhitespace1 *> parseCard)

-- BNF: <remove-card> ::= "remove" <whitespace> <card>
parseRemoveCard :: Parser Lib1.Command
parseRemoveCard = Lib1.RemoveCard <$> (parseString "remove" *> parseWhitespace1 *> parseCard)

-- BNF: <show-deck> ::= "show" <whitespace> "deck"
parseShowDeck :: Parser Lib1.Command
parseShowDeck = parseString "show" *> parseWhitespace1 *> parseString "deck" *> pure Lib1.ShowDeck

-- BNF: <count-cards> ::= "count" <whitespace> "cards"
parseCountCards :: Parser Lib1.Command
parseCountCards = parseString "count" *> parseWhitespace1 *> parseString "cards" *> pure Lib1.CountCards

-- BNF: <play-card> ::= "play" <whitespace> <card>
parsePlayCard :: Parser Lib1.Command
parsePlayCard = Lib1.PlayCard <$> (parseString "play" *> parseWhitespace1 *> parseCard)

-- BNF: <dump> ::= "dump" <whitespace> "examples"
parseDump :: Parser Lib1.Command
parseDump = parseString "dump" *> parseWhitespace1 *> parseString "examples" *> pure (Lib1.Dump Lib1.Examples)

-- BNF: <simple-command> ::= <create-deck> | <shuffle-deck> | <draw-cards> | 
--                           <add-card> | <remove-card> | <show-deck> | 
--                           <count-cards> | <play-card> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = 
    parseCreateDeck <|>
    parseShuffleDeck <|>
    parseDrawCards <|>
    parseAddCard <|>
    parseRemoveCard <|>
    parseShowDeck <|>
    parseCountCards <|>
    parsePlayCard <|>
    parseDump

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence = Parser $ \input -> case runParser parseSimpleCommand input of
    Left err -> Left err
    Right (cmd, rest) -> case runParser parseWhitespace rest of
        Right (_, ';':rest2) -> case runParser (parseWhitespace *> parseSequence) rest2 of
            Right (cmd2, rest3) -> Right (Lib1.Sequence cmd cmd2, rest3)
            Left err -> Left err
        Right (_, rest2) -> Right (cmd, rest2)
        Left err -> Left err

parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- ============================================================================
-- State Definition
-- ============================================================================

data State = State {
    stateDeck :: [Lib1.Card]
} deriving (Show)

-- Eq instances for Lib1 types (needed for State Eq)
instance Eq Lib1.Card where
    (Lib1.Card s1 r1) == (Lib1.Card s2 r2) = s1 == s2 && r1 == r2

instance Eq State where
    (State deck1) == (State deck2) = deck1 == deck2

emptyState :: State
emptyState = State { stateDeck = [] }

-- ============================================================================
-- Business Logic
-- ============================================================================

-- Pure function to execute a command on state
executeCommand :: Lib1.Command -> State -> IO State
executeCommand Lib1.CreateDeck _ = return $ State { stateDeck = fullDeck }
executeCommand Lib1.ShuffleDeck state = do
    shuffled <- shuffleList (stateDeck state)
    return $ state { stateDeck = shuffled }
executeCommand (Lib1.DrawCards n) state = do
    let drawn = take n (stateDeck state)
    let remaining = drop n (stateDeck state)
    putStrLn $ "Drew " ++ show (length drawn) ++ " card(s): " ++ show drawn
    return $ state { stateDeck = remaining }
executeCommand (Lib1.AddCard card) state = 
    return $ state { stateDeck = stateDeck state ++ [card] }
executeCommand (Lib1.RemoveCard card) state = 
    return $ state { stateDeck = filter (/= card) (stateDeck state) }
executeCommand Lib1.ShowDeck state = do
    putStrLn $ "Deck contains " ++ show (length (stateDeck state)) ++ " cards:"
    mapM_ print (stateDeck state)
    return state
executeCommand Lib1.CountCards state = do
    putStrLn $ "Total cards in deck: " ++ show (length (stateDeck state))
    return state
executeCommand (Lib1.PlayCard card) state = do
    if card `elem` stateDeck state
    then do
        putStrLn $ "Played card: " ++ show card
        return $ state { stateDeck = filter (/= card) (stateDeck state) }
    else do
        putStrLn $ "Card not in deck: " ++ show card
        return state
executeCommand (Lib1.Sequence cmd1 cmd2) state = do
    state1 <- executeCommand cmd1 state
    executeCommand cmd2 state1
executeCommand (Lib1.Dump Lib1.Examples) state = do
    putStrLn "Examples:"
    mapM_ (putStrLn . toCliCommand) Lib1.examples
    return state

-- Execute with STM
execute :: TVar State -> Lib1.Command -> IO ()
execute tvar cmd = do
    oldState <- atomically $ readTVar tvar
    newState <- executeCommand cmd oldState
    atomically $ writeTVar tvar newState

-- ============================================================================
-- Helper Functions
-- ============================================================================

fullDeck :: [Lib1.Card]
fullDeck = [Lib1.Card s r | s <- allSuits, r <- allRanks]
  where
    allSuits = [Lib1.Hearts, Lib1.Clubs, Lib1.Spades, Lib1.Diamonds]
    allRanks = [Lib1.Ace, Lib1.Two, Lib1.Three, Lib1.Four, Lib1.Five, Lib1.Six, 
                Lib1.Seven, Lib1.Eight, Lib1.Nine, Lib1.Ten, Lib1.Jack, Lib1.Queen, Lib1.King]

shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList xs = do
    idx <- randomRIO (0, length xs - 1)
    let (before, picked:after) = splitAt idx xs
    rest <- shuffleList (before ++ after)
    return (picked : rest)

-- ============================================================================
-- State Serialization
-- ============================================================================

stateToCommands :: State -> [Lib1.Command]
stateToCommands state = 
    if null (stateDeck state)
    then []
    else Lib1.CreateDeck : map Lib1.AddCard (drop 52 (stateDeck state))
         ++ if length (stateDeck state) < 52 
            then map Lib1.RemoveCard (filter (`notElem` stateDeck state) fullDeck)
            else []

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

-- ============================================================================
-- Storage Operations
-- ============================================================================

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
        op <- readChan chan
        case op of
            Save content responseChan -> do
                writeFile "state.txt" content
                writeChan responseChan ()
                loop
            Load responseChan -> do
                content <- Strict.readFile "state.txt" `catch` handleNotFound
                writeChan responseChan content
                loop
    handleNotFound :: IOError -> IO String
    handleNotFound _ = return ""

save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan tvar = do
    state <- atomically $ readTVar tvar
    let commands = stateToCommands state
    let content = unlines $ map toCliCommand commands
    responseChan <- newChan
    writeChan chan (Save content responseChan)
    _ <- readChan responseChan
    return $ Right ()

load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan tvar = do
    responseChan <- newChan
    writeChan chan (Load responseChan)
    content <- readChan responseChan
    if null content || all null (lines content)
    then do
        -- Empty or non-existent file, use empty state
        atomically $ writeTVar tvar emptyState
        return $ Right ()
    else case parseAndExecuteCommands content of
        Left err -> return $ Left err
        Right state -> do
            atomically $ writeTVar tvar state
            return $ Right ()

parseAndExecuteCommands :: String -> Either String State
parseAndExecuteCommands content = 
    let commandLines = filter (not . null) $ lines content
        parsedCommands = map (runParser parseCommand) commandLines
    in case sequence parsedCommands of
        Left err -> Left err
        Right commands -> Right $ foldl applyCommand emptyState (map fst commands)
  where
    applyCommand :: State -> Lib1.Command -> State
    applyCommand state Lib1.CreateDeck = State { stateDeck = fullDeck }
    applyCommand state (Lib1.AddCard card) = state { stateDeck = stateDeck state ++ [card] }
    applyCommand state (Lib1.RemoveCard card) = state { stateDeck = filter (/= card) (stateDeck state) }
    applyCommand state (Lib1.Sequence cmd1 cmd2) = applyCommand (applyCommand state cmd1) cmd2
    applyCommand state _ = state
