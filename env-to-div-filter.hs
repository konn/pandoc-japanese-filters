{-# LANGUAGE OverloadedStrings, PatternGuards, ViewPatterns #-}
module Main where

import           Data.Default           (def)
import qualified Data.Text              as T
import           Text.LaTeX             (render)
import           Text.LaTeX.Base.Parser (parseLaTeX)
import           Text.LaTeX.Base.Syntax (LaTeX (..))
import           Text.LaTeX.Utils       (stripTeX)
import           Text.Pandoc            (readLaTeX)
import           Text.Pandoc            (bottomUp)
import           Text.Pandoc.Definition (Block (..), Pandoc (..))
import           Text.Pandoc.JSON       (toJSONFilter)
import           Text.Pandoc.Options    (ReaderOptions (..))

main :: IO ()
main = toJSONFilter envToDiv

envToDiv :: Block -> Block
envToDiv (RawBlock "latex" src)
  | Right lat <- parseLaTeX (T.pack src)
  , TeXEnv name args body <- stripTeX lat =
  let Pandoc _ bs = readLaTeX def { readerParseRaw = True } $
                    T.unpack $ render body
  in Div ([], [name], [("data-tex-args", T.unpack $ render args)]) $
     map (bottomUp envToDiv) bs
envToDiv b = b
