{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts, LambdaCase          #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction            #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import           Control.Applicative       ((<*))
import           Control.Applicative       ((<$>))
import           Control.Effect            (EffectState, evalState, get, modify)
import           Control.Effect            (runEffect, runWriter, tell)
import           Control.Monad             (forM_)
import           Control.Monad             (void)
import           Control.Monad.Effect      (Effect)
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS (encodeString)
import           Shelly                    (cd, cp, findWhen, hasExt, mkdir_p)
import           Shelly                    (pwd, shelly, withTmpDir, writefile)
import           Shelly                    ((<.>), (</>))
import           Shelly                    (cmd)
import           Shelly                    (silently)
import           Shelly                    (liftIO)
import           Text.LaTeX                (document, documentclass)
import           Text.LaTeX                (execLaTeXM, render, usepackage)
import           Text.LaTeX                (execLaTeXT)
import           Text.LaTeX.Base.Class     (fromLaTeX)
import           Text.LaTeX.Base.Parser    (parseLaTeX)
import           Text.LaTeX.Base.Syntax    (LaTeX (..))
import           Text.LaTeX.Base.Syntax    (matchCommand)
import           Text.LaTeX.Base.Syntax    (TeXArg (..))
import           Text.LaTeX.QQ             (hat, hat')
import           Text.LaTeX.Utils          (stripTeX)
import           Text.Pandoc               (Inline (..), MathType (..))
import           Text.Pandoc               (Pandoc (..), bottomUpM)
import           Text.Pandoc               (Block (..))
import           Text.Pandoc.JSON          (toJSONFilter)

default (T.Text, Double)

main :: IO ()
main = toJSONFilter processMath

newIdent :: EffectState Integer r => Effect r Integer
newIdent = get <* modify succ

toSVGName :: Integer -> String
toSVGName n = encodeString $ "maths" </> ("math-" ++ show n) <.> "svg"

splitDisplayMath :: Block -> [Block]
splitDisplayMath = undefined

data MathSetting = MathSetting
                   { mathMode :: MathType
                   , mathBody :: String
                   } deriving (Read, Show, Eq, Ord)


b2i :: Num a => Bool -> a
b2i False = 0
b2i True  = 1

mathToTag :: MathType -> Integer -> String -> Inline
mathToTag mode n src =
  let labeled = either (const False) (not . null . matchCommand (`elem` ["tag", "label"])) $
                parseLaTeX $ T.pack src
      indented = "&" `T.isInfixOf` T.pack src
      offset = b2i (labeled || indented) + b2i (mode == DisplayMath)
      linum = T.count "\\\\" (T.pack src) + 1 + offset
  in Span ("", [show mode], [])
     [RawInline "html" $ "<img src=\""++ toSVGName n ++ "\" style=\"height:"++show linum ++"em\" />"]

data Math = Equation LaTeX
          | FitchProof LaTeX
          | GentzenProof LaTeX
          deriving (Show, Eq)

processMath :: Pandoc -> IO Pandoc
processMath pan = do
  let (pan', maths) = runEffect $ runWriter $ evalState 1 $ flip bottomUpM pan $ \case
        RawInline "latex" src
          | Right [hat'|\pboxy{\hask{lat}}|] <- parseLaTeX $ T.pack src
          , [hat'|$\hask{math}$|] <- stripTeX lat -> do
            n <- newIdent
            tell [(n, MathSetting InlineMath $ T.unpack $ render math)]
            return $ mathToTag InlineMath n src
        Math mode math -> do
            n <- newIdent
            tell [(n, MathSetting mode math)]
            return $ mathToTag mode n math
        i -> return i
  shelly $ do
    cwd <- pwd
    let dist = cwd </> "maths"
    mkdir_p dist
    src <- render <$> (execLaTeXT $ do
          documentclass ["leqno"] "bxjsarticle"
          usepackage ["active", "xetex", "tightpage"] "preview"
          usepackage [] "mymacros"
          usepackage [] "amsmath,amssymb"
          usepackage ["inline"] "enumitem"
          usepackage [] "bm"
          usepackage [] "zxjatype"
          usepackage ["kozuka4"] "zxjafont"
          usepackage [] "zxotf"
          usepackage [] "cases"
          usepackage [] "fitch"
          usepackage [] "picins"
          fromLaTeX [hat|\def\fCenter{\ \vdash\ }|]
          document $ forM_ maths $ \(_, MathSetting mode cont) -> do
            liftIO $ print cont
            let Right lat = parseLaTeX $ T.pack cont
                labeled = not (null $ matchCommand (`elem` ["tag", "label"]) lat)
                math | mode == InlineMath = [hat|$\hask{lat}$|]
                     | TeXEnv "aligned" [] body <- lat =
                        TeXEnv "minipage" [FixArg [hat|25 \jsZw|]] $ TeXEnv (if labeled then "align" else "align*") [] body
                     | labeled = TeXEnv "minipage" [FixArg [hat|25 \jsZw|]] $ TeXEnv "align" [] lat
                     | labeled = TeXEnv "minipage" [FixArg [hat|25 \jsZw|]] $ TeXEnv "align*" [] lat
                     | otherwise = TeXEnv "minipage" [FixArg [hat|25 \jsZw|]] $ [hat|\[ \hask{lat} \]|]
            fromLaTeX $ TeXEnv "preview" [] math)
    let tmp = cwd </> "tmp"
    mkdir_p tmp
    withTmpDir $ \_tmp -> do
      let texfile = tmp </> "math.tex"
      cd tmp
      writefile texfile src
      void $ silently $ cmd "xelatex" "-shell-escape" "-halt-on-error"
                            "-file-line-error" "-interaction=nonstopmode"
                            texfile
      void $ cmd "pdf2svg" (tmp </> "math.pdf") (tmp </> "math-%d.svg") "all"
      mapM_ (flip cp dist) =<< findWhen (return . hasExt "svg") tmp
      return ()
  return pan'


