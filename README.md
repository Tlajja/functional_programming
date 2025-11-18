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

## Lib3.hs
---
## State Persistence

### Overview

This application maintains persistent state across sessions by saving the current deck state to a file (state.txt) and loading it on startup. The state is represented as a sequence of CLI commands that can recreate the exact state when executed.

# State Data Structure
``` haskell
data State = State {
    stateDeck :: [Card]
}
```

## Command Mapping Details

| Command | Persisted? | Reason |
|---------|-----------|---------|
| `create deck` | Yes | Creates the base 52-card deck |
| `shuffle deck` | Yes (implicitly) | The resulting order is saved via the add commands |
| `draw <n>` | Yes (implicitly) | Result is captured by the smaller deck |
| `add <card>` | Yes | Directly modifies deck composition |
| `remove <card>` | Yes (implicitly) | Card absence is captured in final deck |
| `play <card>` | Yes (implicitly) | Effect is same as remove, captured in final deck |
| `show deck` | No | Query command, doesn't modify state |
| `count cards` | No | Query command, doesn't modify state |
| `dump examples` | No | Meta command, doesn't modify state |
| `sequence` | N/A | Commands within sequence are evaluated individually |

## Examples

### Example 1: Deck After Drawing Cards

**Initial State:**
```
stateDeck = [Card Hearts Ten, Card Clubs Four, Card Hearts Ace, Card Hearts Ace]
```

This represents a deck where:
- Started with a full 52-card deck
- Shuffled it
- Drew 50 cards (only 2 remain from original deck)
- Added 2 Aces of Hearts

**Generated Commands (state.txt):**
```
create deck
remove Ace of Hearts
remove Two of Hearts
remove Three of Hearts
... (all 52 remove commands to empty the deck)
remove Queen of Diamonds
remove King of Diamonds
add Ten of Hearts
add Four of Clubs
add Ace of Hearts
add Ace of Hearts
```

**Explanation:**
- Start with `create deck` (52 cards)
- Remove all 52 cards to get an empty deck
- Add back the 4 cards in their exact order
- This preserves both which cards are present AND their order

### Example 2: Full Standard Deck

**Initial State:**
```
stateDeck = [all 52 standard cards in standard order]
```

**Generated Commands (state.txt):**
```
create deck
```

**Explanation:**
- Since the deck contains exactly the 52 standard cards in standard order, only `create deck` is needed
- This is the most efficient representation

### Example 3: Shuffled Deck

**Initial State:**
```
stateDeck = [all 52 cards in shuffled random order]
```

**Generated Commands (state.txt):**
```
create deck
remove Ace of Hearts
remove Two of Hearts
... (all 52 remove commands)
add Seven of Diamonds
add Queen of Clubs
add Ace of Spades
... (all 52 add commands in shuffled order)
```

**Explanation:**
- After shuffling, the order matters
- We preserve the exact shuffled order by removing all cards and adding them back in the shuffled sequence
- When loaded, the deck will have the exact same card order as before the save

## Demonstration Screenshots

### Session 1: Creating and Shuffling Deck
<img width="1879" height="1116" alt="snapshot_2025-11-18_23-12-28" src="https://github.com/user-attachments/assets/93c23925-9f5c-4fdf-9462-dba7a67f6cc4" />
*Created deck, shuffled it, and displayed the shuffled order*

### Session 2: State Persisted After Restart
<img width="1879" height="1116" alt="snapshot_2025-11-18_23-12-49" src="https://github.com/user-attachments/assets/08fe89c6-3eb0-46ee-95e5-e575f2d54261" />

*Restarted the program - the exact same shuffled order is preserved!*

### Saved State File
<img width="1879" height="1116" alt="snapshot_2025-11-18_23-13-45" src="https://github.com/user-attachments/assets/1ec274a1-0479-42d6-83d6-930564faffe4" />

*The state.txt file contains commands that recreate the exact deck state*
