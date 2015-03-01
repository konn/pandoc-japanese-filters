{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, ViewPatterns #-}
module Main where
import           Control.Arrow          ((>>>))
import           Data.Default
import           Data.List              (intercalate)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Text.LaTeX             (LaTeX, render)
import           Text.LaTeX.Base.Parser (parseLaTeX)
import           Text.LaTeX.QQ          (hat')
import           Text.Pandoc            (readLaTeX, writeHtmlString, writeICML)
import           Text.Pandoc            (writeLaTeX)
import           Text.Pandoc.Builder    (fromList)
import           Text.Pandoc.Builder    (doc)
import           Text.Pandoc.Builder    (Inlines, plain)
import           Text.Pandoc.Definition (Format, Inline (RawInline, Span))
import           Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))
import           Text.Pandoc.JSON       (toJSONFilter)
import           Text.Pandoc.Options    (Extension (..), ReaderOptions (..))
import           Text.Pandoc.Options    (WriterOptions (..))
import           Text.Pandoc.Shared     (stringify)
import           Text.XML.HXT.Core      (addAttrl, catA, hasName, ifA, isElem)
import           Text.XML.HXT.Core      (none, processBottomUp, processChildren)
import           Text.XML.HXT.Core      (runLA, sattr, this)
import           Text.XML.HXT.Core      (writeDocumentToString, xread)
import qualified Text.XML.HXT.Core      as XML

main :: IO ()
main = toJSONFilter procJapanese

pandocify :: Inlines -> Pandoc
pandocify = doc . plain

toHtml :: Inlines -> String
toHtml = writeHtmlString myWriterOpts . pandocify

toLaTeX :: Inlines -> String
toLaTeX = writeLaTeX myWriterOpts . pandocify

toICML :: Inlines -> String
toICML = writeICML myWriterOpts . pandocify

myWriterExts :: S.Set Extension
myWriterExts = S.union (S.fromList [Ext_ignore_line_breaks
                              ,Ext_raw_html
                              , Ext_raw_tex]) $ writerExtensions def

myWriterOpts :: WriterOptions
myWriterOpts = def { writerExtensions  = myWriterExts  }

data TenMode = Sesame | Dot
             deriving (Read, Show, Eq, Ord)

data JapaneseMarkup = Ruby [(String, String)] [([Inline], [Inline])]
                    | Kenten TenMode [Inline]
                    | TatechuYoko [Inline]
                    deriving (Read, Show, Eq, Ord)

htmlLike :: Format -> Bool
htmlLike = (`elem` ["epub", "epub3", "html", "html5"])

renderJapanese :: Maybe Format -> JapaneseMarkup -> Inline
renderJapanese (Just fmt) i | htmlLike fmt
  = case i of
      Ruby _opts rtbs ->
        RawInline "html" $ concat $ "<ruby>" : map (\(rt, bs) ->
                                                     concat [ toHtml $ fromList bs
                                                            , "<rt>", toHtml $ fromList rt, "</rt>"
                                                            ]) rtbs
                                    ++["</ruby>"]
      Kenten Sesame inl ->
        Span ([], ["goma"], []) inl
      Kenten Dot inl ->
        Span ([], ["bou"], []) inl
      TatechuYoko inl ->
        Span ("", ["tcy"], []) inl
renderJapanese (Just "latex") i
  = case i of
      Ruby _opts rtbs ->
        let mode | length rtbs <= 1 = "g"
                 | otherwise = "m"
        in  RawInline "latex" $ concat [ "\\ruby[", mode, "]{", concatMap (toLaTeX . fromList . fst) rtbs, "}{",
                                         intercalate "|" $ map (toLaTeX . fromList . snd) rtbs, "}"]
      Kenten _ il ->
        RawInline "latex" $ concat ["\\bou{", toLaTeX $ fromList il, "}"]
      TatechuYoko il ->
        RawInline "latex" $ concat ["\\rensuji{", toLaTeX $ fromList il, "}"]
renderJapanese (Just "icml") mark
  = case mark of
    Ruby _opts rtbs ->
      let kind | length rtbs == 1 = "GroupRuby"
               | otherwise = "PerCharacterRuby"
      in encloseICML [("RubyFlag", "1")
                     ,("RubyString", intercalate " " $ map (stringify.snd) rtbs)
                     ,("RubyType", kind)]
                  $ concatMap fst rtbs
    Kenten Dot il ->
      encloseICML [("KentenKind", "kentenSmallBlackCircle")] il
    Kenten Sesame il ->
      encloseICML [("KentenKind", "KentenSesameDot")] il
    TatechuYoko il ->
      encloseICML [("Tatechuyoko", "true")] il

renderJapanese _ _ = error "Japanese format not supported!"

inlify :: Pandoc -> [Inline]
inlify (Pandoc _ bs0) = concatMap go bs0
  where
    go (Plain is) = is
    go (Para is) = is
    go (RawBlock _ _) = []
    go (CodeBlock _ _) = []
    go (BlockQuote bs) = concatMap go bs
    go (BulletList bs) = concatMap (concatMap go) bs
    go (OrderedList _ bs) = concatMap (concatMap go) bs
    go (DefinitionList bs) = concatMap (\(is, b) -> is ++ concatMap (concatMap go) b) bs
    go (Header _ _ b) = b
    go HorizontalRule = []
    go (Table _ _ _ _ _) = []
    go (Div _ bs) = concatMap go bs
    go Null = []

latToInl :: LaTeX -> [Inline]
latToInl = inlify . readLaTeX myReaderOpts . T.unpack . render

procJapanese :: Maybe Format -> Inline -> Inline
procJapanese mfmt (Span (_, ["ruby"], atts) [rt, base])
  = renderJapanese mfmt $ Ruby atts [([rt], [base])]
procJapanese mfmt (RawInline "latex" src)
  | Right [hat'|\ruby[\hask{mode}]{\hask{base}}{\hask{rt}}|] <- parseLaTeX (T.pack src)
  = let modestr = T.unpack (render  mode)
        isGroup = 'g' `elem` modestr
        isMono  = 'm' `elem` modestr
        isJuku  = 'j' `elem` modestr
        args | isGroup = [(latToInl rt, latToInl base)]
             | isMono || isJuku =
                 zip (map (return . Str . T.unpack) $ T.splitOn "|" $ T.pack $ stringify $ latToInl rt)
                     (map (return . Str . (:[])) $ stringify $ latToInl base)
             | otherwise = [(latToInl rt, latToInl base)]
    in renderJapanese mfmt $ Ruby [] args
  | Right [hat'|\ruby{\hask{base}}{\hask{rt}}|] <- parseLaTeX (T.pack src)
  = renderJapanese mfmt $ Ruby [] [(latToInl rt, latToInl base)]
procJapanese mfmt (Span (_, ["sesame"], _) is)
  = renderJapanese mfmt $ Kenten Sesame is
procJapanese mfmt (Span (_, ["bullet"], _) is)
  = renderJapanese mfmt $ Kenten Dot is
procJapanese mfmt (RawInline "latex" src)
  | Right [hat'|\bou{\hask{rt}}|] <- parseLaTeX (T.pack src)
  = let toI = inlify . readLaTeX myReaderOpts . T.unpack . render
    in renderJapanese mfmt $ Kenten Sesame $ toI rt
procJapanese mfmt (Span (_, ["tcy"], _) is)
  = renderJapanese mfmt $ TatechuYoko is
procJapanese mfmt (RawInline "latex" src)
  | Right [hat'|\rensuji{\hask{rt}}|] <- parseLaTeX (T.pack src)
  = let toI = inlify . readLaTeX myReaderOpts . T.unpack . render
    in renderJapanese mfmt $ TatechuYoko $ toI rt
procJapanese _ i = i

myReaderOpts :: ReaderOptions
myReaderOpts = def { readerParseRaw = True
                   , readerApplyMacros = False
                   }


type Attrs = [(String, String)]

encloseICML :: Attrs -> [Inline] -> Inline
encloseICML args body =
  let src = toICML $ fromList body
  in RawInline "icml" $
     concat $ runLA (xread >>> hasName "ParagraphStyleRange" >>> enc
                           >>> processChildren (ifA (hasName "Br") none this)
                           >>> writeDocumentToString []) src
  where
    enc = processBottomUp (trans `XML.when` isCSR)
    isCSR = isElem >>> hasName "CharacterStyleRange"
    trans = addAttrl (catA $ map (uncurry sattr) args)
