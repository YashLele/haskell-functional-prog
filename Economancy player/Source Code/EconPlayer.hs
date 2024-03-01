import qualified Data.ByteString as BS
import JSONDataTypes
import EconDataTypes
import System.Exit (exitSuccess)
import System.IO
import System.Random
import Data.Map
import Data.Maybe (fromJust)

playInvest :: State -> Int -> Int
playInvest stt rnd = case player stt of Nothing -> 0          -- Should not happen in investing phase
                                        Just plrIndex -> rnd `mod` (1 + coins (players stt !! plrIndex))

playAttackOrShield :: State -> Int -> Int
playAttackOrShield stt rnd =
    case player stt of Nothing -> 0  -- Should not happen in attacking phase
                       Just plrIndex -> let playableCards = Prelude.filter (isUsable . snd) (zip [0..] (cards (players stt !! plrIndex)))
                                            playableCards2 = if fromJust (player stt) == attacker (phase stt) then Prelude.filter (canAttack . snd) playableCards else playableCards
                                        in fst (playableCards2 !! (rnd `mod` length playableCards2))

playBuy :: State -> Int -> String
playBuy stt rnd =
    case player stt of Nothing -> "Pass"  -- Should not happen in buy phase
                       Just plrIndex -> let availableCoins = coins (players stt !! plrIndex)
                                            currentShop = shop stt
                                            buyableCards = Data.Map.filterWithKey (\ct _ -> cost ct <= availableCoins) (Data.Map.filter (> 0) currentShop)
                                            in if Data.Map.null buyableCards then "Pass" else
                                                let sz = size buyableCards
                                                    idx = rnd `mod` (sz + 1)
                                                in if idx == sz then "Pass" else produceCardName (keys buyableCards !! idx)

-- main loop without Random Number Generation
{-main :: IO ()
main = do
    jsonstate <- BS.getLine
    state <- parseInput jsonstate
    let flag = False
    let stt = getState state
    case phsname (sttphase state) of "investing" -> print [playInvest stt]
                                     "attacking" -> print [playAttackOrShield stt]
                                     "buy" -> print [playBuy stt]
                                     "end" -> exitSuccess
    main-}

main :: IO ()
main = do
    jsonstate <- BS.getLine
    state <- parseInput jsonstate
    let stt = getState state
    rng <- getStdGen
    let (prn, newRng) = random rng
    case phsname (sttphase state) of "investing" -> print [playInvest stt prn] >> hFlush stdout
                                     "attacking" -> print [playAttackOrShield stt prn] >> hFlush stdout
                                     "buy" -> print [playBuy stt prn] >> hFlush stdout
                                     "end" -> exitSuccess
    setStdGen newRng
    main