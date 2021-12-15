module CommandLine
  ( FileArgs(..)
  , Input(..)
  , StdinArgs(..)
  , runArgs
  )
where

import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           DConf.Data
import           Options.Applicative
import           Paths_dconf2nix                ( version )

data Input = FileInput FileArgs | StdinInput StdinArgs

data FileArgs = FileArgs
  { fileInput :: InputFilePath
  , fileOutput :: OutputFilePath
  , fileRoot :: Root
  , fileTimeout :: ProcessTimeout
  , fileVerbosity :: Verbosity
  }

data StdinArgs = StdinArgs
  { stdinRoot :: Root
  , stdinTimeout :: ProcessTimeout
  , stdinVerbosity :: Verbosity
  }

timeoutArgs :: Parser ProcessTimeout
timeoutArgs = ProcessTimeout <$> option auto
  (long "timeout" <> short 't' <> showDefault <> value 5 <> help
    "Timeout in seconds for the conversion process"
  )

verbosityArgs :: Parser Verbosity
verbosityArgs =
  flag Normal Verbose (long "verbose" <> help "Verbose mode (debug)")

rootArgs :: Parser Root
rootArgs = Root <$> strOption
  (long "root" <> short 'r' <> value T.empty <> help
    "Custom root path. e.g.: system/locale/"
  )

fileArgs :: Parser Input
fileArgs = fmap FileInput $ FileArgs
    <$> (InputFilePath <$> strOption
          (long "input" <> short 'i' <> help "Path to the dconf file (input)")
        )
    <*> (OutputFilePath <$> strOption
          (long "output" <> short 'o' <> help
            "Path to the Nix output file (to be created)"
          )
        )
    <*> rootArgs
    <*> timeoutArgs
    <*> verbosityArgs

stdinArgs :: Parser Input
stdinArgs =
  StdinInput <$> (StdinArgs <$> rootArgs <*> timeoutArgs <*> verbosityArgs)

versionInfo :: String
versionInfo = unlines
  [ ""
  , "██████╗  ██████╗ ██████╗ ███╗   ██╗███████╗██████╗ ███╗   ██╗██╗██╗  ██╗"
  , "██╔══██╗██╔════╝██╔═══██╗████╗  ██║██╔════╝╚════██╗████╗  ██║██║╚██╗██╔╝"
  , "██║  ██║██║     ██║   ██║██╔██╗ ██║█████╗   █████╔╝██╔██╗ ██║██║ ╚███╔╝ "
  , "██║  ██║██║     ██║   ██║██║╚██╗██║██╔══╝  ██╔═══╝ ██║╚██╗██║██║ ██╔██╗ "
  , "██████╔╝╚██████╗╚██████╔╝██║ ╚████║██║     ███████╗██║ ╚████║██║██╔╝ ██╗"
  , "╚═════╝  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚══════╝╚═╝  ╚═══╝╚═╝╚═╝  ╚═╝"
  , ""
  , "Version: " <> showVersion version
  , "Maintainer: Gabriel Volpe (https://gvolpe.com)"
  , "Source code: https://github.com/gvolpe/dconf2nix"
  ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption versionInfo
  (long "version" <> short 'v' <> help "Show the current version")

runArgs :: IO Input
runArgs =
  let
    opts = info
      (helper <*> versionOpt <*> (stdinArgs <|> fileArgs))
      (  fullDesc
      <> progDesc
           "Convert a dconf file into a Nix file, as expected by Home Manager."
      <> header "dconf2nix - Nixify dconf configuration files"
      )
  in  execParser opts
