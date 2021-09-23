import matplotlib as plt
from dataclasses import dataclass
from icecream import ic

# Take a pile of cards. Split them.
# Take one card from each pile to make a new pile.
# Bulgarian Solitaire https://en.wikipedia.org/wiki/Bulgarian_solitaire
# If any pile has no cards, delete it.
# What is the sequence? Are there periodic cycles?
# What card piles are impossible? How many piles will be made?


@dataclass
class Run:
    '''Object for the tracking of a complete hand of solitaire.'''
    numCards    : int
    stopAt      : int
    repeatFrom  : int
    maxPiles    : int

Deck = list[list[int]]
SeenAtIndex = int

def piles(x : int) -> Deck:
    # The split is to just take one card, but that can be adjusted
    cut = 1
    return [list(range(1,cut+1)),(list(range(cut+1,x+1)))]

def takeAcard(xs : Deck) -> Deck:
    return [[n.pop() for n in xs]] + list(filter(None, xs))

def pileSize(xs : list[Deck]) -> Deck:
    return sorted(map(len,xs))

def stopHands(x : int) -> tuple[SeenAtIndex, Deck]:
    hands = [pileSize(piles(x))]
    aHand = takeAcard(piles(x))
    while (ps := pileSize(aHand)) not in hands:
        hands.append(ps)
        aHand = takeAcard(aHand)
    hands.append(ps)
    return (hands.index(ps) + 1,hands)

def oneRun(x : int) -> Run:
    fstS, sndS = stopHands(x)
    return Run(x, len(sndS), fstS, max(map(len,sndS)))

def runs(start : int, stop: int) -> list[Run]:
    r = [oneRun(x) for x in range(start,stop)]
    return r

if __name__ == "__main__":
    # number of cards to simulate
    start : int = 3
    stop  : int = 15
    allRuns = runs(start, stop)
    for r in allRuns:
        print(f"{r.numCards} cards. Index {r.stopAt} = index {r.repeatFrom}. Cycle length {r.stopAt - r. repeatFrom}. Maximum piles {r.maxPiles}.")


