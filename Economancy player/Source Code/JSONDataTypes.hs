module JSONDataTypes where

import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import GHC.Generics ( Generic )
import RIO ( ByteString, (>>>) )

data JSONCard = JSONCard {
    crdname :: String,
    crduses :: Int
} deriving (Eq, Show, Generic)

data JSONPlayer = JSONPlayer {
    plrcoins :: Int,
    plrbuys :: Int,
    plrcards :: [JSONCard]
} deriving (Eq, Show, Generic)

data JSONPhase = JSONPhase {
    phsname :: String,
    phsattacker :: Maybe Int,
    phsattacker_card :: Maybe JSON.Value,
    phswinner :: Maybe Int
} deriving (Eq, Show, Generic)

data JSONShop = JSONShop {
    board_of_Monopoly :: Int,
    incantation :: Int,
    worker :: Int,
    magic_Bean_Stock :: Int,
    bubble :: Int,
    ghost :: Int,
    senior_Worker :: Int,
    gold_Fish :: Int
} deriving (Eq, Show, Generic)

data JSONState = JSONState {
    sttday :: Int,
    sttphase :: JSONPhase,
    sttshop :: JSONShop,
    sttplayers :: [JSONPlayer],
    sttplayer :: Maybe Int
} deriving (Eq, Show, Generic)

jsonOptions :: String -> JSON.Options
jsonOptions fieldPrefix = JSON.defaultOptions {JSON.fieldLabelModifier = Prelude.drop (length fieldPrefix)}

shopOptions :: JSON.Options
shopOptions =
    let uppercaseFirstCharacter [] = []
        uppercaseFirstCharacter (c : rest) = toUpper c : rest
        underscoreToSpace [] = []
        underscoreToSpace (c : rest) = (if c == '_' then ' ' else c) : underscoreToSpace rest
    in JSON.defaultOptions {JSON.fieldLabelModifier = uppercaseFirstCharacter >>> underscoreToSpace}

instance FromJSON JSONCard where
    parseJSON = JSON.genericParseJSON $ jsonOptions "crd"

instance FromJSON JSONPlayer where
    parseJSON = JSON.genericParseJSON $ jsonOptions "plr"

instance FromJSON JSONPhase where
    parseJSON = JSON.genericParseJSON $ jsonOptions "phs"

instance FromJSON JSONShop where
    parseJSON = JSON.genericParseJSON shopOptions

instance FromJSON JSONState where
    parseJSON = JSON.genericParseJSON $ jsonOptions "stt"

parseInput :: ByteString -> IO JSONState
parseInput state = do
    case JSON.decodeStrict state of
        Nothing -> error ""
        Just stt -> return stt

--{"day":1,"phase":{"name":"investing"},"player":0,"players":[{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0}],"coins":2},{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0}],"coins":2}],"shop":{"Board of Monopoly":4,"Bubble":2,"Ghost":4,"Gold Fish":2,"Incantation":6,"Magic Bean Stock":2,"Senior Worker":4,"Worker":4}}

--{"day":3,"phase":{"attacker":1,"attacker_card":false,"name":"attacking"},"player":1,"players":[{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Magic Bean Stock","uses":0}],"coins":1},{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Worker","uses":0}],"coins":1}],"shop":{"Board of Monopoly":4,"Bubble":2,"Ghost":4,"Gold Fish":2,"Incantation":6,"Magic Bean Stock":1,"Senior Worker":4,"Worker":3}}

--{"day":3,"phase":{"attacker":1,"attacker_card":1,"name":"attacking"},"player":0,"players":[{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Magic Bean Stock","uses":0}],"coins":1},{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Worker","uses":1}],"coins":1}],"shop":{"Board of Monopoly":4,"Bubble":2,"Ghost":4,"Gold Fish":2,"Incantation":6,"Magic Bean Stock":1,"Senior Worker":4,"Worker":3}}

--{"day":2,"phase":{"name":"buy"},"player":1,"players":[{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0}],"coins":1},{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0}],"coins":2}],"shop":{"Board of Monopoly":4,"Bubble":2,"Ghost":4,"Gold Fish":2,"Incantation":6,"Magic Bean Stock":2,"Senior Worker":4,"Worker":4}}

--{"day":3,"phase":{"name":"end","winner":0},"players":[{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Magic Bean Stock","uses":1},{"name":"Senior Worker","uses":1},{"name":"Worker","uses":0},{"name":"Ghost","uses":1},{"name":"Ghost","uses":0},{"name":"Worker","uses":1},{"name":"Gold Fish","uses":0},{"name":"Ghost","uses":0},{"name":"Gold Fish","uses":0},{"name":"Senior Worker","uses":1}],"coins":8},{"buys":0,"cards":[],"coins":0}],"shop":{"Board of Monopoly":2,"Bubble":2,"Ghost":1,"Gold Fish":0,"Incantation":6,"Magic Bean Stock":0,"Senior Worker":0,"Worker":0}}

-- main :: IO ()
-- main = do
--     putStrLn "Enter the JSON State";
--     jsonstate <- BS.getLine;
--     state <- parseInput jsonstate;
--     -- print (phsattacker state)
--     -- print (phsattacker_card state)
--     print (sttday state)
--     print (sttphase state)
--     print (sttshop state)
--     print (sttplayers state)
--     print (sttplayer state)