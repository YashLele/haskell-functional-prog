module EconDataTypes where

import JSONDataTypes
import qualified Data.Aeson as JSON
import Data.Scientific (toBoundedInteger)
import Data.Map (Map, fromList)
import Data.Maybe
import RIO.Time (DayOfWeek(Thursday))

data CardType = SOR | MON | INC | WAL | WOR | MAG | BUB | GHO | SEN | FIS | APP | THU | SHI | GOL   -- MON = Board of Monopoly, FIS = Gold Fish, GOL = Golem
    deriving (Eq, Ord)

getCardType :: String -> CardType
getCardType s =
    case s of "Sorcerer's Stipend" -> SOR
              "Board of Monopoly"  -> MON
              "Incantation"        -> INC
              "Wall of Wealth"     -> WAL
              "Worker"             -> WOR
              "Magic Bean Stock"   -> MAG
              "Bubble"             -> BUB
              "Ghost"              -> GHO
              "Senior Worker"      -> SEN
              "Gold Fish"          -> FIS
              "Apprentice"         -> APP
              "Thug"               -> THU
              "Shield of Greed"    -> SHI
              "Golem"              -> GOL

produceCardName :: CardType -> String
produceCardName s =
    case s of SOR -> "Sorcerer's Stipend"
              MON -> "Board of Monopoly"
              INC -> "Incantation"
              WAL -> "Wall of Wealth"
              WOR -> "Worker"
              MAG -> "Magic Bean Stock"
              BUB -> "Bubble"
              GHO -> "Ghost"
              SEN -> "Senior Worker"
              FIS -> "Gold Fish"
              APP -> "Apprentice"
              THU -> "Thug"
              SHI -> "Shield of Greed"
              GOL -> "Golem"

data Card = Card {
    name :: CardType,
    uses :: Int
}

getCard :: JSONCard -> Card
getCard jcrd =
    let name = getCardType (crdname jcrd)
        uses = crduses jcrd
    in Card { name, uses}

data Player = Player {
    coins :: Int,
    buys :: Int,
    cards :: [Card]
}

getPlayer :: JSONPlayer -> Player
getPlayer jplr =
    let coins = plrcoins jplr
        buys = plrbuys jplr
        cards = Prelude.map getCard (plrcards jplr)
    in Player {coins, buys, cards}

data Phase =
    InvestingPhase |
    AttackingPhase {attacker :: Int, attacker_card :: Maybe Int} |
    BuyPhase |
    EndPhase {winner :: Int}

getPhase :: JSONPhase -> Phase
getPhase jphs =
    let name = phsname jphs
    in case name of "investing" -> InvestingPhase
                    "attacking" -> let attacker = case phsattacker jphs of Just i -> i
                                                                           Nothing -> -1
                                       attacker_card = case phsattacker_card jphs of Just i -> case i of JSON.Bool b -> Nothing
                                                                                                         JSON.Number n -> toBoundedInteger n
                                                                                     Nothing -> error "Attacking phase must specify attacker card or false"
                                    in AttackingPhase attacker attacker_card
                    "buy" -> BuyPhase
                    "end" -> let winner = case phswinner jphs of Just i -> i
                                                                 Nothing -> error "End phase must specify a winner"
                              in EndPhase {winner}

type Shop = Map CardType Int

getShop :: JSONShop -> Shop
getShop jshop = fromList [(MON, board_of_Monopoly jshop),
                          (INC, incantation jshop),
                          (WAL, wall_of_Wealth jshop),
                          (WOR, worker jshop),
                          (MAG, magic_Bean_Stock jshop),
                          (BUB, bubble jshop),
                          (GHO, ghost jshop),
                          (SEN, senior_Worker jshop),
                          (FIS, gold_Fish jshop),
                          (APP, apprentice jshop),
                          (THU, thug jshop),
                          (SHI, shield_of_Greed jshop),
                          (GOL, golem jshop)]

data State = State {
    day :: Int,
    phase :: Phase,
    shop :: Shop,
    players :: [Player],
    player :: Maybe Int
}

getState :: JSONState -> State
getState jstate =
    let day = sttday jstate
        phase = getPhase (sttphase jstate)
        shop = getShop (sttshop jstate)
        players = Prelude.map getPlayer (sttplayers jstate)
        player = sttplayer jstate
    in State {day, phase, shop, players, player}

-- calling the following functions on a state with end phase will result in runtime error because 'player' is Nothing

getMe :: State -> Player
getMe stt = players stt !! fromJust (player stt)

getOthers :: State -> [Player]
getOthers stt = let (before, after) = splitAt (fromJust (player stt)) (players stt)
                 in (before ++ drop 1 after)

--othersCoins :: State -> [Int]
--othersCoins stt = Prelude.map coins (others stt)

--othersCards :: State -> [Card]
--othersCards stt = concatMap cards (others stt)