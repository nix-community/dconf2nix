module CommandLine
  ( Args(..)
  , runArgs
  )
where

import           DConf.Data                     ( InputFilePath(..)
                                                , OutputFilePath(..)
                                                , Verbosity(..)
                                                )
import           Options.Applicative

data Args = Args
  { input :: InputFilePath
  , output :: OutputFilePath
  , verbose :: Verbosity
  }

args :: Parser Args
args =
  Args
    <$> (InputFilePath <$> strOption
          (long "input" <> short 'i' <> help "Path to the dconf file (input)")
        )
    <*> (OutputFilePath <$> strOption
          (long "output" <> short 'o' <> help
            "Path to the Nix output file (to be created)"
          )
        )
    <*> flag Normal Verbose (long "verbose" <> help "Verbose mode (debug)")

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
  , "Version: 0.0.1"
  , "Maintainer: Gabriel Volpe"
  , "Source code: https://github.com/gvolpe/dconf2nix"
  ]

version :: Parser (a -> a)
version = infoOption versionInfo
  (long "version" <> short 'v' <> help "Show the current version")

runArgs :: IO Args
runArgs = execParser opts
 where
  opts = info
    (helper <*> version <*> args)
    (  fullDesc
    <> progDesc
         "Convert a dconf file into a Nix file, as expected by Home Manager."
    <> header "dconf2nix - Convert dconf files to Nix"
    )
