module CommandLine
  ( Args(..)
  , runArgs
  )
where

import           DConf.Data                     ( InputFilePath(..)
                                                , OutputFilePath(..)
                                                )
import           Options.Applicative

data Args = Args
  { input :: InputFilePath
  , output :: OutputFilePath
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

runArgs :: IO Args
runArgs = execParser opts
 where
  opts = info
    (args <**> helper)
    (  fullDesc
    <> progDesc
         "Convert a dconf file into a Nix file, as expected by Home Manager."
    <> header "dconf2nix - Convert dconf files to Nix"
    )
