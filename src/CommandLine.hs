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
  , fileEmojiSupport :: EmojiSupport
  , fileVerbosity :: Verbosity
  }

data StdinArgs = StdinArgs
  { stdinRoot :: Root
  , stdinEmojiSupport :: EmojiSupport
  , stdinVerbosity :: Verbosity
  }

verbosityArgs :: Parser Verbosity
verbosityArgs =
  flag Normal Verbose (long "verbose" <> help "Verbose mode (debug)")

emojiArgs :: Parser EmojiSupport
emojiArgs =
  flag Disabled Enabled (long "emoji" <> short 'e' <> help "Enable emoji support (adds a bit of overhead)")

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
    <*> emojiArgs
    <*> verbosityArgs

stdinArgs :: Parser Input
stdinArgs =
  StdinInput <$> (StdinArgs <$> rootArgs <*> emojiArgs <*> verbosityArgs)

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
