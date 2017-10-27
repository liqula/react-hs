{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains combinators for creating DOM React elements.
--
-- The design of creating 'ReactElement's is loosly based on
-- <https://hackage.haskell.org/package/lucid lucid>.
-- Most of the combinators in this module have a type:
--
-- @
-- p_ :: 'Term' 'JSString' eventHandler arg result => arg -> result
-- @
--
-- but you should interpret this as 'p_' having either of the following two types:
--
-- @
-- p_ :: ['PropertyOrHandler' eventHandler] -> 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- p_ :: 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- @
--
-- In the first, 'p_' takes a list of properties and handlers plus the child element(s).  In the
-- second, the list of properties and handlers is omitted. The 'Term' class allows GHC to
-- automatically select the appropriate type.
--
-- Be aware that in React, there are some
-- <https://facebook.github.io/react/docs/dom-differences.html differences> between the browser DOM
-- objects/properties and the properties and attributes you pass to React, as well as React only
-- supports  <https://facebook.github.io/react/docs/tags-and-attributes.html certain attributes>.
-- Event handlers can be created by the combinators in "React.Flux.PropertiesAndEvents".
--
-- Elements not covered by this module can be created manually using 'el'.  But React
-- only supports <https://facebook.github.io/react/docs/tags-and-attributes.html certain elements>
-- and they should all be covered by this module.
--
-- >ul_ $ do li_ (b_ "Hello")
-- >         li_ "World"
-- >         li_ $
-- >             ul_ (li_ "Nested" <> li_ "List")
--
-- would build something like
--
-- ><ul>
-- >  <li><b>Hello</b><li>
-- >  <li>World</li>
-- >  <li><ul>
-- >    <li>Nested</li>
-- >    <li>List</li>
-- >  </ul></li>
-- ></ul>
module React.Flux.DOM where

import React.Flux.Internal
import Data.JSString (JSString)

-- | This class allows the DOM combinators to optionally take a list of properties or handlers, or
-- for the list to be omitted.
--
-- This module provides instances for the supported DOM elements via 'el'.
-- In the case where you have a different collection of React elements
-- (perhaps from a third party library) you can provide a different label.
-- This allows you to create similarly typed instances as below,
-- but with a different term-level function for creating the React modules.
--
-- For example, if you have a function
--
-- @
-- lookupModule :: 'JSString' -> ['PropertyOrHandler' eventHandler] -> 'ReactElementM' eventHandler a -> 'ReactElementM' eventHandler a
-- lookupModule = 'React.Flux.Combinators.foreignClass' . js_someLookupFunction
-- @
--
-- you can create a @newtype MyReactName@ around @'Data.JSString.JSString'@ allowing you to create instances for
--
-- @
-- (child ~ 'ReactElementM' eventHandler a) => 'Term' MyReactName eventHandler ['PropertyOrHandler eventHandler'] (child -> 'ReactElementM' eventHandler a)
-- @
--
-- and so on.

class Term label eventHandler arg result | result -> arg, result -> eventHandler where
    term :: label -> arg -> result

instance (child ~ ReactElementM_ eventHandler a, eventHandler ~ EventHandlerType handler)
      => Term JSString eventHandler [PropertyOrHandler_ eventHandler] (child -> ReactElementM_ eventHandler a) where
    term name props = el name props

instance(eventHandler ~ EventHandlerType handler)
      => Term JSString eventHandler (ReactElementM_ eventHandler a) (ReactElementM_ eventHandler a) where
    term name child = el name [] child

term' :: Term JSString eventHandler arg result => JSString -> arg -> result
term' = term

-- | Some terms are _void elements_, and cannot contain child elements.
-- 
-- See <https://w3c.github.io/html/syntax.html#elements-0>
termVoid :: JSString -> [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
termVoid name p = el name p mempty

-- HTML

a_ :: Term JSString eventHandler arg result => arg -> result; a_ = term' "a"
abbr_ :: Term JSString eventHandler arg result => arg -> result; abbr_ = term' "abbr"
address_ :: Term JSString eventHandler arg result => arg -> result; address_ = term' "address"
area_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); area_ = termVoid "area"
article_ :: Term JSString eventHandler arg result => arg -> result; article_ = term' "article"
aside_ :: Term JSString eventHandler arg result => arg -> result; aside_ = term' "aside"
audio_ :: Term JSString eventHandler arg result => arg -> result; audio_ = term' "audio"
b_ :: Term JSString eventHandler arg result => arg -> result; b_ = term' "b"
base_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); base_ = termVoid "base"
bdi_ :: Term JSString eventHandler arg result => arg -> result; bdi_ = term' "bdi"
bdo_ :: Term JSString eventHandler arg result => arg -> result; bdo_ = term' "bdo"
big_ :: Term JSString eventHandler arg result => arg -> result; big_ = term' "big"
blockquote_ :: Term JSString eventHandler arg result => arg -> result; blockquote_ = term' "blockquote"
body_ :: Term JSString eventHandler arg result => arg -> result; body_ = term' "body"
br_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); br_ = termVoid "br"
button_ :: Term JSString eventHandler arg result => arg -> result; button_ = term' "button"
canvas_ :: Term JSString eventHandler arg result => arg -> result; canvas_ = term' "canvas"
caption_ :: Term JSString eventHandler arg result => arg -> result; caption_ = term' "caption"
cite_ :: Term JSString eventHandler arg result => arg -> result; cite_ = term' "cite"
code_ :: Term JSString eventHandler arg result => arg -> result; code_ = term' "code"
col_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); col_ = termVoid "col"
colgroup_ :: Term JSString eventHandler arg result => arg -> result; colgroup_ = term' "colgroup"
data_ :: Term JSString eventHandler arg result => arg -> result; data_ = term' "data"
datalist_ :: Term JSString eventHandler arg result => arg -> result; datalist_ = term' "datalist"
dd_ :: Term JSString eventHandler arg result => arg -> result; dd_ = term' "dd"
del_ :: Term JSString eventHandler arg result => arg -> result; del_ = term' "del"
details_ :: Term JSString eventHandler arg result => arg -> result; details_ = term' "details"
dfn_ :: Term JSString eventHandler arg result => arg -> result; dfn_ = term' "dfn"
dialog_ :: Term JSString eventHandler arg result => arg -> result; dialog_ = term' "dialog"
div_ :: Term JSString eventHandler arg result => arg -> result; div_ = term' "div"
dl_ :: Term JSString eventHandler arg result => arg -> result; dl_ = term' "dl"
dt_ :: Term JSString eventHandler arg result => arg -> result; dt_ = term' "dt"
em_ :: Term JSString eventHandler arg result => arg -> result; em_ = term' "em"
embed_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); embed_ = termVoid "embed"
fieldset_ :: Term JSString eventHandler arg result => arg -> result; fieldset_ = term' "fieldset"
figcaption_ :: Term JSString eventHandler arg result => arg -> result; figcaption_ = term' "figcaption"
figure_ :: Term JSString eventHandler arg result => arg -> result; figure_ = term' "figure"
footer_ :: Term JSString eventHandler arg result => arg -> result; footer_ = term' "footer"
form_ :: Term JSString eventHandler arg result => arg -> result; form_ = term' "form"
h1_ :: Term JSString eventHandler arg result => arg -> result; h1_ = term' "h1"
h2_ :: Term JSString eventHandler arg result => arg -> result; h2_ = term' "h2"
h3_ :: Term JSString eventHandler arg result => arg -> result; h3_ = term' "h3"
h4_ :: Term JSString eventHandler arg result => arg -> result; h4_ = term' "h4"
h5_ :: Term JSString eventHandler arg result => arg -> result; h5_ = term' "h5"
h6_ :: Term JSString eventHandler arg result => arg -> result; h6_ = term' "h6"
head_ :: Term JSString eventHandler arg result => arg -> result; head_ = term' "head"
header_ :: Term JSString eventHandler arg result => arg -> result; header_ = term' "header"
hr_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); hr_ = termVoid "hr"
html_ :: Term JSString eventHandler arg result => arg -> result; html_ = term' "html"
i_ :: Term JSString eventHandler arg result => arg -> result; i_ = term' "i"
iframe_ :: Term JSString eventHandler arg result => arg -> result; iframe_ = term' "iframe"
img_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); img_ = termVoid "img"
input_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); input_ = termVoid "input"
ins_ :: Term JSString eventHandler arg result => arg -> result; ins_ = term' "ins"
kbd_ :: Term JSString eventHandler arg result => arg -> result; kbd_ = term' "kbd"
keygen_ :: Term JSString eventHandler arg result => arg -> result; keygen_ = term' "keygen"
label_ :: Term JSString eventHandler arg result => arg -> result; label_ = term' "label"
legend_ :: Term JSString eventHandler arg result => arg -> result; legend_ = term' "legend"
li_ :: Term JSString eventHandler arg result => arg -> result; li_ = term' "li"
link_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); link_ = termVoid "link"
main_ :: Term JSString eventHandler arg result => arg -> result; main_ = term' "main"
map_ :: Term JSString eventHandler arg result => arg -> result; map_ = term' "map"
mark_ :: Term JSString eventHandler arg result => arg -> result; mark_ = term' "mark"
menu_ :: Term JSString eventHandler arg result => arg -> result; menu_ = term' "menu"
menuitem_ :: Term JSString eventHandler arg result => arg -> result; menuitem_ = term' "menuitem"
meta_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); meta_ = termVoid "meta"
meter_ :: Term JSString eventHandler arg result => arg -> result; meter_ = term' "meter"
nav_ :: Term JSString eventHandler arg result => arg -> result; nav_ = term' "nav"
noscript_ :: Term JSString eventHandler arg result => arg -> result; noscript_ = term' "noscript"
object_ :: Term JSString eventHandler arg result => arg -> result; object_ = term' "object"
ol_ :: Term JSString eventHandler arg result => arg -> result; ol_ = term' "ol"
optgroup_ :: Term JSString eventHandler arg result => arg -> result; optgroup_ = term' "optgroup"
option_ :: Term JSString eventHandler arg result => arg -> result; option_ = term' "option"
output_ :: Term JSString eventHandler arg result => arg -> result; output_ = term' "output"
p_ :: Term JSString eventHandler arg result => arg -> result; p_ = term' "p"
param_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); param_ = termVoid "param"
picture_ :: Term JSString eventHandler arg result => arg -> result; picture_ = term' "picture"
pre_ :: Term JSString eventHandler arg result => arg -> result; pre_ = term' "pre"
progress_ :: Term JSString eventHandler arg result => arg -> result; progress_ = term' "progress"
q_ :: Term JSString eventHandler arg result => arg -> result; q_ = term' "q"
rp_ :: Term JSString eventHandler arg result => arg -> result; rp_ = term' "rp"
rt_ :: Term JSString eventHandler arg result => arg -> result; rt_ = term' "rt"
ruby_ :: Term JSString eventHandler arg result => arg -> result; ruby_ = term' "ruby"
s_ :: Term JSString eventHandler arg result => arg -> result; s_ = term' "s"
samp_ :: Term JSString eventHandler arg result => arg -> result; samp_ = term' "samp"
script_ :: Term JSString eventHandler arg result => arg -> result; script_ = term' "script"
section_ :: Term JSString eventHandler arg result => arg -> result; section_ = term' "section"
select_ :: Term JSString eventHandler arg result => arg -> result; select_ = term' "select"
small_ :: Term JSString eventHandler arg result => arg -> result; small_ = term' "small"
source_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); source_ = termVoid "source"
span_ :: Term JSString eventHandler arg result => arg -> result; span_ = term' "span"
strong_ :: Term JSString eventHandler arg result => arg -> result; strong_ = term' "strong"
style_ :: Term JSString eventHandler arg result => arg -> result; style_ = term' "style"
sub_ :: Term JSString eventHandler arg result => arg -> result; sub_ = term' "sub"
summary_ :: Term JSString eventHandler arg result => arg -> result; summary_ = term' "summary"
sup_ :: Term JSString eventHandler arg result => arg -> result; sup_ = term' "sup"
table_ :: Term JSString eventHandler arg result => arg -> result; table_ = term' "table"
tbody_ :: Term JSString eventHandler arg result => arg -> result; tbody_ = term' "tbody"
td_ :: Term JSString eventHandler arg result => arg -> result; td_ = term' "td"
textarea_ :: Term JSString eventHandler arg result => arg -> result; textarea_ = term' "textarea"
tfoot_ :: Term JSString eventHandler arg result => arg -> result; tfoot_ = term' "tfoot"
th_ :: Term JSString eventHandler arg result => arg -> result; th_ = term' "th"
thead_ :: Term JSString eventHandler arg result => arg -> result; thead_ = term' "thead"
time_ :: Term JSString eventHandler arg result => arg -> result; time_ = term' "time"
title_ :: Term JSString eventHandler arg result => arg -> result; title_ = term' "title"
tr_ :: Term JSString eventHandler arg result => arg -> result; tr_ = term' "tr"
track_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); track_ = termVoid "track"
u_ :: Term JSString eventHandler arg result => arg -> result; u_ = term' "u"
ul_ :: Term JSString eventHandler arg result => arg -> result; ul_ = term' "ul"
var_ :: Term JSString eventHandler arg result => arg -> result; var_ = term' "var"
video_ :: Term JSString eventHandler arg result => arg -> result; video_ = term' "video"
wbr_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); wbr_ = termVoid "wbr"


-- SVG

circle_ :: Term JSString eventHandler arg result => arg -> result; circle_ = term' "circle"
clipPath_ :: Term JSString eventHandler arg result => arg -> result; clipPath_ = term' "clipPath"
defs_ :: Term JSString eventHandler arg result => arg -> result; defs_ = term' "defs"
ellipse_ :: Term JSString eventHandler arg result => arg -> result; ellipse_ = term' "ellipse"
g_ :: Term JSString eventHandler arg result => arg -> result; g_ = term' "g"
image_ :: Term JSString eventHandler arg result => arg -> result; image_ = term' "image"
line_ :: Term JSString eventHandler arg result => arg -> result; line_ = term' "line"
linearGradient_ :: Term JSString eventHandler arg result => arg -> result; linearGradient_ = term' "linearGradient"
mask_ :: Term JSString eventHandler arg result => arg -> result; mask_ = term' "mask"
path_ :: Term JSString eventHandler arg result => arg -> result; path_ = term' "path"
pattern_ :: Term JSString eventHandler arg result => arg -> result; pattern_ = term' "pattern"
polygon_ :: Term JSString eventHandler arg result => arg -> result; polygon_ = term' "polygon"
polyline_ :: Term JSString eventHandler arg result => arg -> result; polyline_ = term' "polyline"
radialGradient_ :: Term JSString eventHandler arg result => arg -> result; radialGradient_ = term' "radialGradient"
rect_ :: Term JSString eventHandler arg result => arg -> result; rect_ = term' "rect"
stop_ :: Term JSString eventHandler arg result => arg -> result; stop_ = term' "stop"
svg_ :: Term JSString eventHandler arg result => arg -> result; svg_ = term' "svg"
text_ :: Term JSString eventHandler arg result => arg -> result; text_ = term' "text"
tspan_ :: Term JSString eventHandler arg result => arg -> result; tspan_ = term' "tspan"
