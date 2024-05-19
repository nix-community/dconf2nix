module CommandLine
  ( Args(..)
  , FileArgs(..)
  , Input(..)
  , runArgs
  )
where

import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           DConf.Data
import           Options.Applicative
import           Paths_dconf2nix                ( version )

data Args = Args
  { argsRoot :: Root
  , argsStyle :: Style
  , argsVerbosity :: Verbosity
  , argsInput :: Input
  }

data Input = FileInput FileArgs | StdinInput

data FileArgs = FileArgs
  { fileInput :: InputFilePath
  , fileOutput :: OutputFilePath
  }

styleArgs :: Parser Style
styleArgs =
  flag' HomeManager (long "home-manager" <> help "Use NixOS syntax")
  <|> flag' NixOS (long "nixos" <> help "Use home-manager syntax")

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

stdinArgs :: Parser Input
stdinArgs = pure StdinInput

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
  , "Maintainers: Nix Community"
  , "Source code: https://github.com/nix-community/dconf2nix"
  ]

versionOpt :: Parser (a -> a)
versionOpt = infoOption versionInfo
  (long "version" <> short 'v' <> help "Show the current version")

runArgs :: IO Args
runArgs =
  let
    args = Args <$> rootArgs <*> styleArgs <*> verbosityArgs <*> (stdinArgs <|> fileArgs)

    opts = info
      (helper <*> versionOpt <*> args)
      (  fullDesc
      <> progDesc
           "Convert a dconf file into a Nix file, as expected by Home Manager."
      <> header "dconf2nix - Nixify dconf configuration files"
      )
  in  execParser opts
