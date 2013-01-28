This library is used for playing around with different configurations for teh game 'Resistance'.

Right now we only have the most basic functions, creating a game consisting of either 'Spy' or 'Resistance' members.

The eventual goal is to be able to use this to data mine actual played games. Using a smartphone frontend and this Haskell library as the backend. 

Example:

Let's say that you wanted to find out the odds of the hammer being a Resistance member, in an 8 person game, when you're the leader (and also Resistance). 

```haskell
chanceOfRHammer = NRLeaderAndHammer / NRLeader
    where rLeader = filter leaderIsResist $ makeSetOfGames 8
          leaderIsResist g  = (allegiance  $ leader g) == Resistance
          NRLeader          = genericLength rLeader
          NRLeaderAndHammer = genericLength $ filter hammerIsResist rLeader
          hammerIsResist g  = (allegiance $ hammer g) == Resistance
```
