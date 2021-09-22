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
    stopAt      : float
    repeatFrom  : int
    maxPiles    : int

# number of cards to simulate
start : int = 3
stop  : int = 500

cards : list = list(range(start,stop))

def piles(x : int) -> list[list[int]]:
    # The split is to just take one card, but that can be adjusted
    cut = 1
    return [list(range(1,cut+1)),(list(range(cut+1,x+1)))]

def takeAcard(xs : list[list[int]]) -> list[list[int]]:
    heads = []
    tails = []
    for x in xs:
        if (h := x[0]) != []:
            heads.append(h)
        if (t := x[1:]) != []:
            tails.append(t)
    flat_tails = flattened = [i for sublist in tails for i in sublist]
    return [heads, flat_tails]


# have to finish the rest!
ic(piles(10))
ic(takeAcard(piles(10)))


