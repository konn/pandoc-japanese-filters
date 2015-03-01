Pandoc representation of Japanese markups
=========================================

We express Japanese-specific markups with `Span`{.haskell} constructor.
Currently we support following markups:

* Ruby (振り仮名; Furigana),
* Horizontals-in-Vertical alignment (縦中横; Tate-chu-yoko),
* and Side dot emphases (圏点 or 傍点; Kenten or Bouten).

Ruby
----
Ruby with body "北海道" and pronounce "ほっかいどう" is expressed as follows:

```haskell
Span ("", ["ruby"], attrs) ["ほっかいどう", "北海道"]
```

We store additional format specific attribute information
in `attrs`{.haskell}.

Tatechuyoko
-----------
Tatechuyoko strings are renered holizontally in vertical writing environment.
It is just annotated with `"tcy"`{.haskell} class:

```haskell
Para [Str "本日は第", Span ("", ["tcy"], []) [Str "42"], Str "回大会にご参加頂きありがとうございます"]
```

Side-dot emhases
----------------
**This spec is likely to change, because of the lack of extensibility**

Currently two types of side-dots are supported: Sesame (ゴマ「、」) and Bullet (黒丸「・」)。
These are just expressed as class name:

```haskell
Span ("", ["sesame"], []) [Str "ゴマ付きです"]
Span ("", ["bullet"], []) [Str "黒丸付きです"]
```

### Future plan for side-dot representation ###
It might be flexible representing different kind of side-dots
with the same class-name?

