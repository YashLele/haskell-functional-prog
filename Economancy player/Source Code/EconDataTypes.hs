module EconDataTypes where

import JSONDataTypes
import qualified Data.Aeson as JSON
import Data.Scientific (toBoundedInteger)
import Data.Map (Map, fromList)

data CardType = SOR | MON | INC | WOR | MAG | BUB | GHO | SEN | FIS     -- MON = Board of Monopoly, FIS = Gold Fish
    deriving (Eq, Ord)

getCardType :: String -> CardType
getCardType s =
    case s of "Sorcerer's Stipend" -> SOR
              "Board of Monopoly"  -> MON
              "Incantation"        -> INC
              "Worker"             -> WOR
              "Magic Bean Stock"   -> MAG
              "Bubble"             -> BUB
              "Ghost"              -> GHO
              "Senior Worker"      -> SEN
              "Gold Fish"          -> FIS

produceCardName :: CardType -> String
produceCardName s =
    case s of SOR -> "Sorcerer's Stipend"
              MON -> "Board of Monopoly"
              INC -> "Incantation"
              WOR -> "Worker"
              MAG -> "Magic Bean Stock"
              BUB -> "Bubble"
              GHO -> "Ghost"
              SEN -> "Senior Worker"
              FIS -> "Gold Fish"

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
                          (WOR, worker jshop),
                          (MAG, magic_Bean_Stock jshop),
                          (BUB, bubble jshop),
                          (GHO, ghost jshop),
                          (SEN, senior_Worker jshop),
                          (FIS, gold_Fish jshop)]

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

maxUses :: Card -> Int
maxUses _ = 1

isUsable :: Card -> Bool
isUsable c = uses c < maxUses c

canAttack :: Card -> Bool
canAttack c = name c /= BUB

cost :: CardType -> Int
cost ct =
    case ct of SOR -> 0
               MON -> 2
               INC -> 4
               WOR -> 1
               MAG -> 1
               BUB -> 2
               GHO -> 2
               SEN -> 2
               FIS -> 3

-- getCardInfo :: CardType -> CardInfo
-- getCardInfo c =
--     case c of SOR -> CardInfo 0 [2,1,1] 0 0 0
--               MON -> CardInfo 2 [0,0,0] 1 1 1
--               INC -> CardInfo 4 [0,0,0] 3 1 1
--               WOR -> CardInfo 1 [0,1,1] 0 1 2
            --   etc, etc, etc

-- data CardInfo = CardInfo {
--     cost :: Int,
--     rewards :: [Int],
--     vic :: Int,
--     attack :: Int,
--     shield :: Int
-- }

--twoCoinRewardFunction