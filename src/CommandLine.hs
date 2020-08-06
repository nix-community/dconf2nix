module CommandLine
  ( FileArgs(..)
  , Input(..)
  , StdinArgs(..)
  , runArgs
  )
where

import           Data.Version                   ( showVersion )
import           DConf.Data                     ( InputFilePath(..)
                                                , OutputFilePath(..)
                                                , ProcessTimeout(..)
                                                , Verbosity(..)
                                                )
import           Options.Applicative
import           Paths_dconf2nix                ( version )

data Input = FileInput FileArgs | StdinInput StdinArgs

data FileArgs = FileArgs
  { input :: InputFilePath
  , output :: OutputFilePath
  , processTimeout :: ProcessTimeout
  , verbose :: Verbosity
  }

data StdinArgs = StdinArgs
  { stdinTimeout :: ProcessTimeout
  , stdinVerbose :: Verbosity
  }

timeoutArgs :: Parser ProcessTimeout
timeoutArgs = ProcessTimeout <$> option
  auto
  (long "timeout" <> short 't' <> showDefault <> value 5 <> help
    "Timeout in seconds for the conversion process"
  )

verbosityArgs :: Parser Verbosity
verbosityArgs =
  flag Normal Verbose (long "verbose" <> help "Verbose mode (debug)")

fileArgs :: Parser Input
fileArgs =
  fmap FileInput
    $   FileArgs
    <$> (InputFilePath <$> strOption
          (long "input" <> short 'i' <> help "Path to the dconf file (input)")
        )
    <*> (OutputFilePath <$> strOption
          (long "output" <> short 'o' <> help
            "Path to the Nix output file (to be created)"
          )
        )
    <*> timeoutArgs
    <*> verbosityArgs

stdinArgs :: Parser Input
stdinArgs = StdinInput <$> (StdinArgs <$> timeoutArgs <*> verbosityArgs)

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
  , "Maintainer: Gabriel Volpe"
  , "Source code: https://github.com/gvolpe/dconf2nix"
  ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption versionInfo
  (long "version" <> short 'v' <> help "Show the current version")

runArgs :: IO Input
runArgs = execParser opts
 where
  opts = info
    (helper <*> versionOpt <*> (stdinArgs <|> fileArgs))
    (  fullDesc
    <> progDesc
         "Convert a dconf file into a Nix file, as expected by Home Manager."
    <> header "dconf2nix - Nixify dconf configuration files"
    )
