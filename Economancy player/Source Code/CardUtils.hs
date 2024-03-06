module CardUtils where

import EconDataTypes
import Data.Map
import Data.Maybe

data CardDetails = CardDetails {
    cost :: Int,
    attack :: Int,
    defense :: Int,
    victory :: Int,
    income :: Int -> Int -> Int, -- arguments: day, money , returns: number of coins earned on day 
    maxUses :: Int,
    buy :: [Int]
}

cardInfoMap :: Map CardType CardDetails
cardInfoMap = fromList [(SOR, CardDetails{cost=0, attack=0, defense=0, victory=0, income= \day _ -> if day<=1 then 2 else 1, maxUses=1, buy=[0,0,0]}),
                        (MON, CardDetails{cost=2, attack=1, defense=1, victory=1, income= \_ _ -> 0,                         maxUses=1, buy=[0,0,0]}),
                        (INC, CardDetails{cost=4, attack=1, defense=1, victory=3, income= \_ _ -> 0,                         maxUses=1, buy=[0,0,0]}),
                        (WAL, CardDetails{cost=1, attack=0, defense=0, victory=0, income= \day _ -> if day<=1 then 1 else 0, maxUses=2, buy=[0,0,0]}),
                        (WOR, CardDetails{cost=1, attack=0, defense=0, victory=0, income= \day _ -> if day<=1 then 0 else 1, maxUses=1, buy=[0,0,0]}),
                        (MAG, CardDetails{cost=1, attack=0, defense=0, victory=0, income= \_ money -> money `div` 3,         maxUses=1, buy=[0,0,0]}),
                        (BUB, CardDetails{cost=2, attack=0, defense=0, victory=0, income= \_ _ -> 0,                         maxUses=1, buy=[0,0,0]}),
                        (GHO, CardDetails{cost=2, attack=0, defense=0, victory=0, income= \day _ -> if day<=2 then 0 else 1, maxUses=1, buy=[0,0,0]}),
                        (SEN, CardDetails{cost=2, attack=0, defense=0, victory=0, income= \_ _ -> 1,                         maxUses=1, buy=[0,0,0]}),
                        (FIS, CardDetails{cost=3, attack=0, defense=0, victory=0, income= \day _ -> if day<=2 then 0 else 4, maxUses=1, buy=[0,0,0]}),
                        (APP, CardDetails{cost=3, attack=0, defense=0, victory=0, income= \day _ -> if day<=2 then 1 else 0, maxUses=1, buy=[0,0,1]}),
                        (THU, CardDetails{cost=3, attack=0, defense=0, victory=0, income= \day _ -> if day==2 then 1 else 0, maxUses=1, buy=[0,0,0]}),
                        (SHI, CardDetails{cost=4, attack=0, defense=0, victory=0, income= \_ _ -> 0,                         maxUses=1, buy=[0,0,0]}),
                        (GOL, CardDetails{cost=5, attack=0, defense=0, victory=0, income= \_ _ -> 0,                         maxUses=1, buy=[0,0,0]})]

getCardDetails :: Card -> CardDetails
getCardDetails c = fromJust (Data.Map.lookup (name c) cardInfoMap)

canAttack :: Card -> Bool
canAttack c = name c /= BUB

isUsable :: Bool -> Card -> Bool
isUsable isDefending c = if isDefending then uses c < maxUses (getCardDetails c)
                          else uses c < 1

costOf :: CardType -> Int
costOf ct = cost (fromJust (Data.Map.lookup ct cardInfoMap))

victoryPointsOf :: CardType -> Int
victoryPointsOf ct = victory (fromJust (Data.Map.lookup ct cardInfoMap))

-- returns True if first Card's attack and defense are >= second Card's defense and attack
isStrong :: Card -> Card -> Bool
isStrong c1 c2 = let cd1 = getCardDetails c1
                     cd2 = getCardDetails c2
                     in (attack cd1 >= defense cd2) && (defense cd1 >= attack cd2)

--                          day   money
getNextDayIncome :: Card -> Int -> Int -> Int
getNextDayIncome crd day = income (getCardDetails crd) (mod day 3 + 1)