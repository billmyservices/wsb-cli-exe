{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Int (Int64)
import ParseArgs
import System.Exit
import WSB.Client
import qualified WSB.Client.Counter as C
import qualified WSB.Client.CounterType as CT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Conversion as BS
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as B64
import Data.Monoid ((<>))
import Data.Aeson
import Data.List (intercalate)

makeAuthRequest :: Arguments -> ExceptT ByteString IO AuthReq
makeAuthRequest (Arguments url uid skey _) = do
  hmac <- case B64.decode (BS.toByteString' skey) of
            Right x -> return x
            Left  e -> throwError $ "Cannot decode de secret key: " <> BS.toByteString' e
  case reads uid of
    [(x, [])] -> authReq x hmac url
    _         -> throwError $ "Bad `USERID`, number expected but '" <> BS.toByteString' uid <> "' provided. Go to your Bill My Services profile and look for the user id."

runCommand :: AuthReq -> Command -> ExceptT ByteString IO ()

runCommand auth (WsbList format) = counterTypeList auth
                               >>= formatCounterTypeList format
                               >>= (liftIO . putStrLn)

runCommand auth (WsbRead ct format) = counterTypeRead (ctReq auth ct)
                                  >>= formatCounterTypeData format
                                  >>= (liftIO . putStrLn)

runCommand auth (WsbDel ct) = counterTypeDelete (ctReq auth ct)

runCommand auth (WsbAdd ct nm v mi ma mo) = counterTypeAdd (ctReq auth ct) (Text.pack nm) v mi ma mo

runCommand auth (WsbPost ct c v) = counterPost (cReq auth ct c) v

runCommand auth (WsbGet ct c format) = counterRead (cReq auth ct c)
                                   >>= formatCounter format
                                   >>= (liftIO . putStrLn)

runCommand auth (WsbReset ct c) = counterReset (cReq auth ct c)

ctReq auth = CounterTypeReq auth . Text.pack
cReq auth ct = CounterReq (ctReq auth ct) . Text.pack

formatCounterTypeList :: Format -> [CT.CounterType] -> ExceptT ByteString IO String
formatCounterTypeList JSON = asJson
formatCounterTypeList CSV = return . unlines . map counterType2CSV

formatCounterTypeData :: Format -> CounterTypeData -> ExceptT ByteString IO String
formatCounterTypeData JSON x = asJson x
formatCounterTypeData CSV (CounterTypeData ct cs) = return $ unlines $ counterType2CSV ct: map counter2CSV cs

formatCounter :: Format -> C.Counter -> ExceptT ByteString IO String
formatCounter JSON = asJson
formatCounter CSV = return . counter2CSV

asJson :: ToJSON a => a -> ExceptT ByteString IO String
asJson xs = case BS.fromByteString' (encode xs) of
              Nothing -> throwError "cannot encode to JSON"
              Just rs -> return rs

counterType2CSV :: CT.CounterType -> String
counterType2CSV x = intercalate "," [ "CounterType"
                                    , Text.unpack $ CT.code x
                                    , Text.unpack $ CT.name x
                                    , show        $ CT.version x
                                    , show        $ CT.value x
                                    , show        $ CT.k1 x
                                    , show        $ CT.k2 x ]

counter2CSV :: C.Counter -> String
counter2CSV x = intercalate "," [ "Counter"
                                , Text.unpack $ C.code x
                                , show        $ C.value x
                                , show        $ C.timeRef x ]

main :: IO ()
main = do

  args <- parseArgs

  result <- runExceptT $ makeAuthRequest args >>= flip runCommand (argCommand args)

  case result of
    Right _ -> exitSuccess
    Left  e -> do
                  B8.putStrLn e
                  exitFailure

