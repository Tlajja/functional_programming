# fp-2025

## Lesson notes

Can be viewed [here](https://vipo.github.io/fp-2025/)

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop

1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands

1. `stack build`
2. `stack test`

## Lib1.hs

### BNF

```
<Command> ::= "CreateDeck"
            | "ShuffleDeck"
            | "DrawCards" <Int>
            | "AddCard" <Card>
            | "RemoveCard" <Card>
            | "ShowDeck"
            | "CountCards"
            | "PlayCard" <Card>
            | "Sequence" <Command> <Command>
            | "Dump" <Dumpable>

<Card> ::= <Suit> <Rank>
<Rank> ::= "Ace" | "Two" | "Three" | "Four" | "Five" | "Six" | "Seven" | "Eight" | "Nine" | "Ten" | "Jack" | "Queen" | "King"
<Suit> ::= "Hearts" | "Clubs" | "Spades" | "Diamonds"
<Dumpable> ::= "Examples"
```
