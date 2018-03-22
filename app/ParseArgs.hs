module ParseArgs where

import Data.Int (Int64)
import Data.ByteString (ByteString)
import Options.Applicative
import Data.Semigroup ((<>))
import WSB.CounterVersion
import System.Environment
import Data.List (intercalate)

data Format = CSV | JSON deriving (Read, Show, Eq, Ord, Enum)

data Command = WsbList { cmdListFormat  :: Format
                       }
             | WsbRead { cmdReadCtCode  :: String
                       , cmdReadFormat  :: Format
                       }
             | WsbDel  { cmdDelCtCode  :: String
                       }
             | WsbAdd  { cmdAddCtCode   :: String
                       , cmdAddName     :: String
                       , cmdAddValue    :: Int64
                       , cmdAddK1       :: Int64
                       , cmdAddK2       :: Int64
                       , cmdAddVersion  :: CounterVersion
                       }
             | WsbGet  { cmdGetCtCode   :: String
                       , cmdGetCCode    :: String
                       , cmdReadFormat  :: Format
                       }
             | WsbPost { cmdPostCtCode  :: String
                       , cmdPostCCode   :: String
                       , cmdPostValue   :: Int64
                       }
             | WsbReset { cmdResetCtCode  :: String
                        , cmdResetCCode   :: String
                        }
               deriving (Show)

data Arguments = Arguments { argWsbUrl    :: String
                           , argWsbUserId :: String
                           , argWsbSkey   :: String
                           , argCommand   :: Command
                           } deriving Show

parseArgs :: IO Arguments
parseArgs = do

  -- get default values from environment variables
  defaultWsbUrl     <- maybe mempty value <$> lookupEnv "WSB_URL"
  defaultWsbUserId  <- maybe mempty value <$> lookupEnv "WSB_USERID"
  defaultWsbSkey    <- maybe mempty value <$> lookupEnv "WSB_SKEY"

  let cmdList = command "list" $ flip info (progDesc "List your counter types") $
                  WsbList <$> optFormat

      cmdRead = command "read" $ flip info (progDesc "Read one counter type") $
                  WsbRead <$> optCounterType <*> optFormat

      cmdDel = command "delete" $ flip info (progDesc "Delete one counter type and all their counters") $
                  WsbDel <$> optCounterType

      cmdAdd  = command "add"  $ flip info (progDesc "Add one counter type") $
                  WsbAdd <$> optCounterType <*> optName <*> optValue <*> optK1 <*> optK2 <*> optVersion

      cmdGet = command "get" $ flip info (progDesc "Get the current state of one counter") $
                  WsbGet <$> optCounterType <*> optCounter <*> optFormat

      cmdPost = command "post" $ flip info (progDesc "Try to post one counter change") $
                  WsbPost <$> optCounterType <*> optCounter <*> optValue

      cmdReset = command "reset" $ flip info (progDesc "Reset one counter to the default values") $
                   WsbReset <$> optCounterType <*> optCounter

      optCounterType  = argument str  $ metavar "COUNTERTYPECODE" <> help "Your own counter type code"
      optCounter      = argument str  $ metavar "COUNTERCODE"     <> help "Your own counter code"
      optName         = argument str  $ metavar "NAME"            <> help "Counter type name"
      optValue        = argument auto $ metavar "INTEGER"         <> help "Default value for new counters"
      optK1           = argument auto $ metavar "INTEGER"         <> help "first configuration counter value (it depends of version)"
      optK2           = argument auto $ metavar "INTEGER"         <> help "second configuration counter value (it depends of version)"
      optVersion      = argument auto $ metavar "COUNTERVERSION"  <> help ("Counter type version (or mode), " ++ oneOf (undefined :: CounterVersion))
      optFormat       = argument auto $ metavar "FORMAT"          <> help ("Format for output data, " ++ oneOf (undefined :: Format))

      optURL = strOption ( long "url"
                        <> metavar "URL"
                        <> defaultWsbUrl
                        <> help "Bill By Services endpoint, you can set the WSB_URL environment variable" )

      optUserID = strOption ( long "user"
                           <> metavar "USERID"
                           <> defaultWsbUserId
                           <> help "Bill By Services user profile id, you can set the WSB_USERID environment variable" )

      optSKey = strOption ( long "key"
                         <> metavar "SECRETKEY"
                         <> defaultWsbSkey
                         <> help "Bill By Services user profile secret key, you can set the WSB_SKEY environment variable" )

      progArgs = Arguments <$> optURL <*> optUserID <*> optSKey <*> hsubparser (
                    cmdList <> cmdRead <> cmdAdd <> cmdDel <> cmdGet <> cmdPost <> cmdReset )

      withInfo opts desc = info (helper <*> opts) $ progDesc desc

  execParser (progArgs `withInfo` "Bill By Services command line interface")

oneOf :: (Show a, Enum a) => a -> String
oneOf x = "one of: " ++ intercalate ", " (map show xs)
  where xs = [toEnum 0..]
        ys = x: xs
