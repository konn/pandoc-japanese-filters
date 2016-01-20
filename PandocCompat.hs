{-# LANGUAGE CPP #-}
module PandocCompat (readLaTeX, readLaTeX') where
import qualified Text.Pandoc as P

#if MIN_VERSION_pandoc(1,15,0)
readLaTeX = P.readLaTeX
readLaTeX' opt src = case P.readLaTeX opt src of
  Right pan -> pan
  Left err  -> error $ show err
#else
readLaTeX opts = Right . P.readLaTeX opts
readLaTeX' = P.readLaTeX
#endif
