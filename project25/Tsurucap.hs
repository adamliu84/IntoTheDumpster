-- WARN/NOTE/TODO: The following is acting as a basic, it is not coded to the requirement spec asks!
-- TODO: [Remove print (take 9, Error in timestamp precision, -r order requirement]
-- Ref: https://www.tsurucapital.com/en/code-sample.html
-- Special thanks & core ref learning from: https://github.com/iprokin/pcapKospi200
{-
    pcap format description
    https://wiki.wireshark.org/Development/LibpcapFileFormat#File_Format
    https://delog.wordpress.com/2010/12/13/information-in-a-pcap-file-with-a-single-udp-packet/
    https://www.elvidence.com.au/understanding-time-stamps-in-packet-capture-data-pcap-files/
    UDP description
    https://en.wikipedia.org/wiki/User_Datagram_Protocol
    https://en.wikibooks.org/wiki/Communication_Networks/TCP_and_UDP_Protocols
    Reading pcap
    https://serverfault.com/questions/38626/how-can-i-read-pcap-files-in-a-friendly-format
    tcpdump -qns 0 -X -r file.pcap | less
    Wireshark-gtk
    Dealing with Binary in Haskell
    https://wiki.haskell.org/Dealing_with_binary_data
    https://hackage.haskell.org/package/binary-0.9.0.0/docs/Data-Binary-Get.html
    http://hackage.haskell.org/package/binary-0.8.5.1/docs/src/Data.Binary.Get.html#runGetIncremental
-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-} -- Slience this due to incomplete upon constructing QuoteMessage{..}

import Control.Monad ( replicateM )
import Data.Binary ( Word8, Word16, Word32, getWord8, Get )
import Data.Binary.Get
    ( Decoder(..),
      getLazyByteString,
      getWord16be,
      getWord32le,
      runGetIncremental,
      skip )
import qualified Data.ByteString               as BS  (ByteString)
import qualified Data.ByteString.Lazy          as BL (ByteString, readFile, drop)
import qualified Data.ByteString.Lazy.Char8    as CL (unpack)
import qualified Data.ByteString.Lazy.Internal as LI (ByteString (Chunk, Empty),chunk)
import           Data.Either (rights)
import           Data.Int (Int64)
import           Data.Time
import           Data.List (intercalate)
import           Data.List.Split (chunksOf)
import           Data.Time.Clock.POSIX         (posixSecondsToUTCTime)

-- << Const Config
lenGlobalHeader :: Int64
lenGlobalHeader = 24

filename_placeholder :: String
filename_placeholder = "mdf-kospi200.20110216-0.pcap"

idString :: String
idString  = "B6034"

endOfMessage :: Word8
endOfMessage = 255

lenFullQuotePack :: Int
lenFullQuotePack = 215 -- Based on Quote Packet Spec

lenQuotePack_placeholder :: Int
lenQuotePack_placeholder = lenFullQuotePack - 6 -- Skip till IssueCode and omit eom-1byte

lenPcapH :: Int
lenPcapH = 16

lenEthAndIP4 :: Int
lenEthAndIP4 = 34

lenUDPh :: Int
lenUDPh = 8
-- >>

-- << Data
data PcapPacketHeader = PcapPacketHeader
    { timestampSec  :: !Word32
    , timestampUsec :: !Word32
    , capturedLen   :: !Word32
    , originalLen   :: !Word32
    } deriving (Show)

data EthernetHeader = EthernetHeader
    { destMac   :: ![Word8]  -- 6 bytes
    , sourceMac :: ![Word8]  -- 6 bytes
    , etherType :: !Word16
    } deriving (Show)

data IPv4Header = IPv4Header
    { ipVersion     :: !Word8
    , ipTos         :: !Word8
    , ipTotalLength :: !Word16
    , ipId          :: !Word16
    , ipFlags       :: !Word16
    , ipTtl         :: !Word8
    , ipProtocol    :: !Word8 -- NOTE: UDP 17, TCP 6
    , ipChecksum    :: !Word16
    , ipSrcAddr     :: !Word32
    , ipDstAddr     :: !Word32
    } deriving (Show)

data UDPHeader = UDPHeader
    { udpSrcPort  :: !Word16
    , udpDstPort  :: !Word16
    , udpLength   :: !Word16
    , udpChecksum :: !Word16
    } deriving (Show)

data QuoteMessage = QuoteMessage
    {
      qmIssueCode  :: String
    , qmBidsPrice1 :: Double
    , qmBidsVol1   :: Double
    , qmBidsPrice2 :: Double
    , qmBidsVol2   :: Double
    , qmBidsPrice3 :: Double
    , qmBidsVol3   :: Double
    , qmBidsPrice4 :: Double
    , qmBidsVol4   :: Double
    , qmBidsPrice5 :: Double
    , qmBidsVol5   :: Double
    , qmAsksPrice1 :: Double
    , qmAsksVol1   :: Double
    , qmAsksPrice2 :: Double
    , qmAsksVol2   :: Double
    , qmAsksPrice3 :: Double
    , qmAsksVol3   :: Double
    , qmAsksPrice4 :: Double
    , qmAsksVol4   :: Double
    , qmAsksPrice5 :: Double
    , qmAsksVol5   :: Double
    , acceptTime   :: String
    , qmPacketTs   :: Double
    }

-- << Instance
instance Show QuoteMessage where
    show :: QuoteMessage -> String
    -- <pkt-time> <accept-time> <issue-code> <bqty5>@<bprice5> ... <bqty1>@<bprice1> <aqty1>@<aprice1> ... <aqty5>@<aprice5>
    show QuoteMessage{..}  = Data.List.intercalate sep
        [ show $ posixSecondsToUTCTime $ realToFrac qmPacketTs         
        , acceptTime'    
        , qmIssueCode
        , priceVolShow qmBidsPrice5 qmBidsVol5
        , priceVolShow qmBidsPrice4 qmBidsVol4
        , priceVolShow qmBidsPrice3 qmBidsVol3
        , priceVolShow qmBidsPrice2 qmBidsVol2
        , priceVolShow qmBidsPrice1 qmBidsVol1
        , priceVolShow qmAsksPrice1 qmAsksVol1
        , priceVolShow qmAsksPrice2 qmAsksVol2
        , priceVolShow qmAsksPrice3 qmAsksVol3
        , priceVolShow qmAsksPrice4 qmAsksVol4
        , priceVolShow qmAsksPrice5 qmAsksVol5
        ]
        where sep = "\t"
              priceVolShow p v = concat [show v,"@",show p]
              acceptTime' = let
                            [hh,mm,ss,uu] = chunksOf 2 acceptTime
                            in intercalate ":" [hh,mm,ss,uu]
-- >>

-- << Util
newtype Error = Error Int deriving (Show)

error'' :: Int -> QuoteLine
error'' = Left . Error

type QuoteLine = Either Error QuoteMessage  
-- >>

-- << Parser
getPcapPacketHeader :: Get PcapPacketHeader
getPcapPacketHeader = do
    timestampSec  <- getWord32le
    timestampUsec <- getWord32le
    capturedLen   <- getWord32le
    originalLen   <- getWord32le
    return PcapPacketHeader{..}

getEthernetHeader :: Get EthernetHeader
getEthernetHeader = do
    destMac   <- replicateM 6 getWord8 -- 6 bytes
    sourceMac <- replicateM 6 getWord8 -- 6 bytes
    etherType <- getWord16be
    return EthernetHeader{..}

getIPv4Header :: Get IPv4Header
getIPv4Header = do
    ipVersion     <- getWord8
    ipTos         <- getWord8
    ipTotalLength <- getWord16be
    ipId          <- getWord16be
    ipFlags       <- getWord16be
    ipTtl         <- getWord8
    ipProtocol    <- getWord8
    ipChecksum    <- getWord16be
    ipSrcAddr     <- getWord32le
    ipDstAddr     <- getWord32le
    return IPv4Header{..}

getUDPHeader :: Get UDPHeader
getUDPHeader = do
    udpSrcPort    <- getWord16be
    udpDstPort    <- getWord16be
    udpLength     <- getWord16be
    udpChecksum   <- getWord16be
    let uDPHeader' = UDPHeader{..} in
        do return uDPHeader'{udpLength = udpLength - 8}
    -- NOTE: UDP header has size of 8 bytes. Required to deduct
    -- return $ UDPHeader udpSrcPort udpDstPort (udpLength-8) udpChecksum (Testing with above style)

getQuoteMessage :: Get QuoteMessage
getQuoteMessage = do
    qmIssueCode  <- (return.CL.unpack) =<< getLazyByteString 12
    skip 5 -- Issue seq.-no. 3, Market Status Type 2
    skip 7 -- Total bid quote volume 7
    [   qmBidsPrice1,qmBidsVol1
        ,qmBidsPrice2,qmBidsVol2
        ,qmBidsPrice3,qmBidsVol3
        ,qmBidsPrice4,qmBidsVol4
        ,qmBidsPrice5,qmBidsVol5
        ] <- getPriceVol    
    skip 7 -- Total ask quote volume 7
    [   qmAsksPrice1,qmAsksVol1
        ,qmAsksPrice2,qmAsksVol2
        ,qmAsksPrice3,qmAsksVol3
        ,qmAsksPrice4,qmAsksVol4
        ,qmAsksPrice5,qmAsksVol5
        ] <- getPriceVol    
    skip 50 -- No. of best bid/ask valid quotes 50
    acceptTime <- (return.CL.unpack) =<< getLazyByteString 8
    return QuoteMessage{..}
    where
        getPriceVol :: Get [Double]
        getPriceVol = do
            r <- mapM (\_ -> do
                    p <- getLazyByteString 5
                    v <- getLazyByteString 7
                    return [p,v]
                ) [1..5]
            return $ map (\x -> read (CL.unpack x)::Double) (concat r)

getQuoteLine :: Get QuoteLine
getQuoteLine = do
    packetHeader <- getPcapPacketHeader
    let captureLen' = fromIntegral $ capturedLen packetHeader
    case captureLen' == lenEthAndIP4 + lenUDPh + lenFullQuotePack of
        True -> processValidLen packetHeader
        False -> do
            skip captureLen'
            return $ error'' 1
    where
        processValidLen :: PcapPacketHeader -> Get QuoteLine
        processValidLen PcapPacketHeader{..} = do
            getEthernetHeader
            getIPv4Header -- Just read Ether & IPv4 header & ignore >>, instead of skip ()
            udpHeader <- getUDPHeader
            let lenUDP = fromIntegral $ udpLength udpHeader
            dataId     <- getLazyByteString 5
            quoteMessage   <- getQuoteMessage -- skip lenQuotePack_placeholder (Just leave the comment here)
            eOm        <- getWord8
            case CL.unpack dataId == idString
                    && lenUDP == lenFullQuotePack
                    && eOm == endOfMessage of
                True  ->
                    return $ 
                        Right quoteMessage {qmPacketTs= fromIntegral timestampSec + fromIntegral timestampSec * pcapTsUsecUnits}
                False ->
                    return $
                        error'' 2
        pcapTsUsecUnits :: Double
        pcapTsUsecUnits = 1e-6 

-- >>

-- https://hackage.haskell.org/package/binary-0.8.9.3/docs/Data-Binary-Get.html#g:3
basicIncrementProcess :: LI.ByteString -> IO ()
basicIncrementProcess content = do
    let result = rights $ go decoder content
    mapM_ print (take 9 result)
    where
        decoder :: Decoder QuoteLine
        decoder = runGetIncremental getQuoteLine
        go :: Decoder QuoteLine -> BL.ByteString -> [QuoteLine]
        go (Done leftover _consumed trade) input =
            trade : go decoder (LI.chunk leftover input)
        go (Partial k) input                     =
            go (k . takeHeadChunk $ input) (dropHeadChunk input)
        go (Fail _leftover _consumed msg) _input =
            []
        takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
        takeHeadChunk lbs =
            case lbs of
            (LI.Chunk bs _) -> Just bs
            _              -> Nothing
        dropHeadChunk :: BL.ByteString -> LI.ByteString
        dropHeadChunk lbs =
            case lbs of
            (LI.Chunk _ lbs') -> lbs'
            _                -> LI.Empty

main :: IO ()
main = do
    pcapData <- BL.readFile filename_placeholder
    let content = BL.drop lenGlobalHeader pcapData
    basicIncrementProcess content
