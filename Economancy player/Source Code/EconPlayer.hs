import qualified Data.ByteString as BS
import JSONDataTypes
import EconDataTypes
import CardUtils
import System.Exit (exitSuccess)
import System.IO
import System.Random
import Data.List
import Data.Map hiding (splitAt, drop)
import Data.Maybe

playInvest :: State -> Int -> Int
playInvest stt rnd = if even rnd
                      then playInvestSafe (getMe stt) (getOthers stt) (day stt)
                      else rnd `mod` (1 + coins (getMe stt))

-- Invest only if I have a good attacking card (attack >= others' defense, and defense >= others' attack)
--    AND sufficient money (more than others) to win the chance to attack.
-- Otherwise, invest 0.
playInvestSafe :: Player -> [Player] -> Int -> Int
playInvestSafe me others day = let othersCoins = Prelude.map coins others
                                   othersCards = concatMap cards others
                               in if all (\c -> c < coins me) othersCoins && any (\crd -> all (isStrong crd) othersCards) (cards me) then
                                     (if coins me == 1 + maximum othersCoins then
                                        (if any (\c -> 0 < getNextDayIncome c day (coins me)) (cards me) then
                                           1 + maximum othersCoins
                                        else
                                           maximum othersCoins)
                                     else
                                        1 + maximum othersCoins)
                                  else 0

-- If attacking, choose the card that has at least as much attack as the strongest opponent defense
-- If defending, choose the card that has the strongest defense
playAttackOrDefend :: State -> Int -> Int
playAttackOrDefend stt rnd = let defendingCards = Prelude.filter (isUsable True . snd) (zip [0..] (cards (getMe stt)))
                                 attackingCards = Prelude.filter (\ic -> canAttack (snd ic) && isUsable False (snd ic)) defendingCards
                                 in if fromJust (player stt) == attacker (phase stt) then
                                       (if even rnd
                                           then fst (attackingCards !! (rnd `mod` length attackingCards))
                                           else let othersMaxDefense = maximum (Data.List.map (defense . getCardDetails) (concatMap cards (getOthers stt)))
                                                    in fst (head (Prelude.filter (\ic -> attack (getCardDetails (snd ic)) >= othersMaxDefense) attackingCards)))
                                    else
                                       (if even rnd
                                           then fst (defendingCards !! (rnd `mod` length defendingCards))
                                           else let myMaxDefense = maximum (Data.List.map (defense . getCardDetails . snd) defendingCards)
                                                    in fst (head (Prelude.filter (\ic -> defense (getCardDetails (snd ic)) == myMaxDefense) defendingCards)))

-- Prioritize buying cards that earn victory points
playBuy :: State -> Int -> String
playBuy stt rnd = let availableCoins = coins (getMe stt)
                      currentShop = shop stt
                      buyableCards = filterWithKey (\ct _ -> costOf ct <= availableCoins) (Data.Map.filter (> 0) currentShop)
                    in if Data.Map.null buyableCards then "Pass" else
                        let sz = size buyableCards
                            idx = rnd `mod` (sz + 1)
                        in if idx == sz then "Pass" else
                            let victoryCards = Prelude.filter (\ct -> victoryPointsOf ct > 0) (keys buyableCards)
                                maxVictoryCard
                                    | elem INC victoryCards = Just INC
                                    | elem MON victoryCards = Just MON
                                    | otherwise = Nothing
                                in produceCardName (fromMaybe (keys buyableCards !! idx) maxVictoryCard)


main :: IO ()
main = do
    jsonstate <- BS.getLine
    state <- parseInput jsonstate
    let stt = getState state
    rng <- getStdGen
    let (prn, newRng) = random rng
    case phsname (sttphase state) of "investing" -> print [playInvest stt prn] >> hFlush stdout
                                     "attacking" -> print [playAttackOrDefend stt prn] >> hFlush stdout
                                     "buy" -> print [playBuy stt prn] >> hFlush stdout
                                     "end" -> exitSuccess
    setStdGen newRng
    main