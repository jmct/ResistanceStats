module Resistance where

import Data.List (genericLength)

data Allegiance = Resistance
                | Spy
    deriving (Eq, Show)

type GroupMember a = (a, Allegiance)

type Player = GroupMember Int

allegiance :: Player -> Allegiance
allegiance = snd

data Group a = Empty
             | Group {left :: [a], leader :: a, right :: [a]}
    deriving (Show)


--This function allows you to create a group with arbitrary numbers of resistance and spies. 
--To create groups that follow the rulebook's guidelines, see the 'makeSetOfGames' function
possibleGroups :: Int -> Int -> [[Allegiance]]
possibleGroups 0   0   = [[]]
possibleGroups nRs nSs 
    = [x:xs | 
             (x, tails) <- [(Resistance, possibleGroups (pred nRs) nSs) | nRs > 0] ++
                           [(Spy, possibleGroups nRs (pred nSs)) | nSs > 0],
             xs <- tails]

--This takes a list of Player constructors and converts it into a Group data-type
--The reason for defaulting to an empty list for the right-hand side is because
--the usual leadership change moves to the left. 
listToGroup :: [Allegiance] -> Group Player
listToGroup [] = Empty
listToGroup xs = Group ys y []
    where (y:ys) = zip [1..] xs

leftPlayer :: Group Player -> Player
leftPlayer g = case left g of
                    []     -> last $ right g
                    (x:xs) -> x

leftAllegiance :: Group Player -> Allegiance
leftAllegiance = snd . leftPlayer

rightPlayer :: Group Player -> Player
rightPlayer g = case right g of
                    []     -> last $ left g
                    (x:xs) -> x

rightAllegiance :: Group Player -> Allegiance
rightAllegiance = snd . rightPlayer

--This function just shifts the leader to the person to the left of the current leader
nextLeader :: Group Player -> Group Player
nextLeader Empty = Empty
nextLeader g = case left g of
                    []     -> Group newLeft newLeader []
                    (x:xs) -> Group xs x (leader g:(right g))
               where newLeft  = drop 1 tempLeft
                     tempLeft = reverse $ right g
                     newLeader = head tempLeft

--hammer just iterates the 'nextLeader' function 5 times over the group
hammer :: Group Player -> Player
hammer g = leader $ iterate nextLeader g !! 5


--For each group size between 5 and 10 there are a pre-determined number of spies
--This function ensures that when you make a game, the correct number of spies vs
--resistance are used. 
makeSetOfGames :: Int -> [Group Player]
makeSetOfGames num
    | num < 5   = [Empty]
    | num < 7   = map listToGroup $ possibleGroups (num - 2) 2
    | num < 10  = map listToGroup $ possibleGroups (num - 3) 3
    | num == 10 = map listToGroup $ possibleGroups (num - 4) 3 
    | otherwise = error "Too many players :(\n"
