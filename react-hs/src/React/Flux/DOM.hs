-- | This module contains combinators for creating DOM React elements.
--
-- The design of creating 'ReactElement's is loosly based on
-- <https://hackage.haskell.org/package/lucid lucid>.
-- Most of the combinators in this module have a type:
--
-- @
-- p_ :: 'Term' eventHandler arg result => arg -> result
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
-- supports  <https://facebook.github.io/react/docs/tags-and-attributes.html certian attributes>.
-- Event handlers can be created by the combinators in "React.Flux.PropertiesAndEvents".
--
-- Elements not covered by this module can be created manually using 'el'.  But React
-- only supports <https://facebook.github.io/react/docs/tags-and-attributes.html certian elements>
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

-- | This class allows the DOM combinators to optionally take a list of properties or handlers, or
-- for the list to be omitted.
class Term eventHandler arg result | result -> arg, result -> eventHandler where
    term :: JSString -> arg -> result

instance (child ~ ReactElementM eventHandler a) => Term eventHandler [PropertyOrHandler eventHandler] (child -> ReactElementM eventHandler a) where
    term name props = el name props

instance Term eventHandler (ReactElementM eventHandler a) (ReactElementM eventHandler a) where
    term name child = el name [] child

{-
#define node(name) name ## _ :: Term eventHandler arg result => arg -> result; name ## _ = term #name
#define elem(name) name ## _ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); name ## _ p = el #name p mempty

-- Copy the elements from react documentation and use :s/ /)^v^Mnode(/g


-- HTML

node(a)
node(abbr)
node(address)
node(area)
node(article)
node(aside)
node(audio)
node(b)
node(base)
node(bdi)
node(bdo)
node(big)
node(blockquote)
node(body)
elem(br)
node(button)
node(canvas)
node(caption)
node(cite)
node(code)
node(col)
node(colgroup)
node(data)
node(datalist)
node(dd)
node(del)
node(details)
node(dfn)
node(dialog)
node(div)
node(dl)
node(dt)
node(em)
node(embed)
node(fieldset)
node(figcaption)
node(figure)
node(footer)
node(form)
node(h1)
node(h2)
node(h3)
node(h4)
node(h5)
node(h6)
node(head)
node(header)
elem(hr)
node(html)
node(i)
node(iframe)
node(img)
elem(input)
node(ins)
node(kbd)
node(keygen)
node(label)
node(legend)
node(li)
node(link)
node(main)
node(map)
node(mark)
node(menu)
node(menuitem)
node(meta)
node(meter)
node(nav)
node(noscript)
node(object)
node(ol)
node(optgroup)
node(option)
node(output)
node(p)
node(param)
node(picture)
node(pre)
node(progress)
node(q)
node(rp)
node(rt)
node(ruby)
node(s)
node(samp)
node(script)
node(section)
node(select)
node(small)
node(source)
node(span)
node(strong)
node(style)
node(sub)
node(summary)
node(sup)
node(table)
node(tbody)
node(td)
node(textarea)
node(tfoot)
node(th)
node(thead)
node(time)
node(title)
node(tr)
node(track)
node(u)
node(ul)
node(var)
node(video)
node(wbr)


-- SVG

node(circle)
node(clipPath)
node(defs)
node(ellipse)
node(g)
node(image)
node(line)
node(linearGradient)
node(mask)
node(path)
node(pattern)
node(polygon)
node(polyline)
node(radialGradient)
node(rect)
node(stop)
node(svg)
node(text)
node(tspan)
-}

-- HTML

a_ :: Term eventHandler arg result => arg -> result; a_ = term "a"
abbr_ :: Term eventHandler arg result => arg -> result; abbr_ = term "abbr"
address_ :: Term eventHandler arg result => arg -> result; address_ = term "address"
area_ :: Term eventHandler arg result => arg -> result; area_ = term "area"
article_ :: Term eventHandler arg result => arg -> result; article_ = term "article"
aside_ :: Term eventHandler arg result => arg -> result; aside_ = term "aside"
audio_ :: Term eventHandler arg result => arg -> result; audio_ = term "audio"
b_ :: Term eventHandler arg result => arg -> result; b_ = term "b"
base_ :: Term eventHandler arg result => arg -> result; base_ = term "base"
bdi_ :: Term eventHandler arg result => arg -> result; bdi_ = term "bdi"
bdo_ :: Term eventHandler arg result => arg -> result; bdo_ = term "bdo"
big_ :: Term eventHandler arg result => arg -> result; big_ = term "big"
blockquote_ :: Term eventHandler arg result => arg -> result; blockquote_ = term "blockquote"
body_ :: Term eventHandler arg result => arg -> result; body_ = term "body"
br_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); br_ p = el "br" p mempty
button_ :: Term eventHandler arg result => arg -> result; button_ = term "button"
canvas_ :: Term eventHandler arg result => arg -> result; canvas_ = term "canvas"
caption_ :: Term eventHandler arg result => arg -> result; caption_ = term "caption"
cite_ :: Term eventHandler arg result => arg -> result; cite_ = term "cite"
code_ :: Term eventHandler arg result => arg -> result; code_ = term "code"
col_ :: Term eventHandler arg result => arg -> result; col_ = term "col"
colgroup_ :: Term eventHandler arg result => arg -> result; colgroup_ = term "colgroup"
data_ :: Term eventHandler arg result => arg -> result; data_ = term "data"
datalist_ :: Term eventHandler arg result => arg -> result; datalist_ = term "datalist"
dd_ :: Term eventHandler arg result => arg -> result; dd_ = term "dd"
del_ :: Term eventHandler arg result => arg -> result; del_ = term "del"
details_ :: Term eventHandler arg result => arg -> result; details_ = term "details"
dfn_ :: Term eventHandler arg result => arg -> result; dfn_ = term "dfn"
dialog_ :: Term eventHandler arg result => arg -> result; dialog_ = term "dialog"
div_ :: Term eventHandler arg result => arg -> result; div_ = term "div"
dl_ :: Term eventHandler arg result => arg -> result; dl_ = term "dl"
dt_ :: Term eventHandler arg result => arg -> result; dt_ = term "dt"
em_ :: Term eventHandler arg result => arg -> result; em_ = term "em"
embed_ :: Term eventHandler arg result => arg -> result; embed_ = term "embed"
fieldset_ :: Term eventHandler arg result => arg -> result; fieldset_ = term "fieldset"
figcaption_ :: Term eventHandler arg result => arg -> result; figcaption_ = term "figcaption"
figure_ :: Term eventHandler arg result => arg -> result; figure_ = term "figure"
footer_ :: Term eventHandler arg result => arg -> result; footer_ = term "footer"
form_ :: Term eventHandler arg result => arg -> result; form_ = term "form"
h1_ :: Term eventHandler arg result => arg -> result; h1_ = term "h1"
h2_ :: Term eventHandler arg result => arg -> result; h2_ = term "h2"
h3_ :: Term eventHandler arg result => arg -> result; h3_ = term "h3"
h4_ :: Term eventHandler arg result => arg -> result; h4_ = term "h4"
h5_ :: Term eventHandler arg result => arg -> result; h5_ = term "h5"
h6_ :: Term eventHandler arg result => arg -> result; h6_ = term "h6"
head_ :: Term eventHandler arg result => arg -> result; head_ = term "head"
header_ :: Term eventHandler arg result => arg -> result; header_ = term "header"
hr_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); hr_ p = el "hr" p mempty
html_ :: Term eventHandler arg result => arg -> result; html_ = term "html"
i_ :: Term eventHandler arg result => arg -> result; i_ = term "i"
iframe_ :: Term eventHandler arg result => arg -> result; iframe_ = term "iframe"
img_ :: Term eventHandler arg result => arg -> result; img_ = term "img"
input_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler (); input_ p = el "input" p mempty
ins_ :: Term eventHandler arg result => arg -> result; ins_ = term "ins"
kbd_ :: Term eventHandler arg result => arg -> result; kbd_ = term "kbd"
keygen_ :: Term eventHandler arg result => arg -> result; keygen_ = term "keygen"
label_ :: Term eventHandler arg result => arg -> result; label_ = term "label"
legend_ :: Term eventHandler arg result => arg -> result; legend_ = term "legend"
li_ :: Term eventHandler arg result => arg -> result; li_ = term "li"
link_ :: Term eventHandler arg result => arg -> result; link_ = term "link"
main_ :: Term eventHandler arg result => arg -> result; main_ = term "main"
map_ :: Term eventHandler arg result => arg -> result; map_ = term "map"
mark_ :: Term eventHandler arg result => arg -> result; mark_ = term "mark"
menu_ :: Term eventHandler arg result => arg -> result; menu_ = term "menu"
menuitem_ :: Term eventHandler arg result => arg -> result; menuitem_ = term "menuitem"
meta_ :: Term eventHandler arg result => arg -> result; meta_ = term "meta"
meter_ :: Term eventHandler arg result => arg -> result; meter_ = term "meter"
nav_ :: Term eventHandler arg result => arg -> result; nav_ = term "nav"
noscript_ :: Term eventHandler arg result => arg -> result; noscript_ = term "noscript"
object_ :: Term eventHandler arg result => arg -> result; object_ = term "object"
ol_ :: Term eventHandler arg result => arg -> result; ol_ = term "ol"
optgroup_ :: Term eventHandler arg result => arg -> result; optgroup_ = term "optgroup"
option_ :: Term eventHandler arg result => arg -> result; option_ = term "option"
output_ :: Term eventHandler arg result => arg -> result; output_ = term "output"
p_ :: Term eventHandler arg result => arg -> result; p_ = term "p"
param_ :: Term eventHandler arg result => arg -> result; param_ = term "param"
picture_ :: Term eventHandler arg result => arg -> result; picture_ = term "picture"
pre_ :: Term eventHandler arg result => arg -> result; pre_ = term "pre"
progress_ :: Term eventHandler arg result => arg -> result; progress_ = term "progress"
q_ :: Term eventHandler arg result => arg -> result; q_ = term "q"
rp_ :: Term eventHandler arg result => arg -> result; rp_ = term "rp"
rt_ :: Term eventHandler arg result => arg -> result; rt_ = term "rt"
ruby_ :: Term eventHandler arg result => arg -> result; ruby_ = term "ruby"
s_ :: Term eventHandler arg result => arg -> result; s_ = term "s"
samp_ :: Term eventHandler arg result => arg -> result; samp_ = term "samp"
script_ :: Term eventHandler arg result => arg -> result; script_ = term "script"
section_ :: Term eventHandler arg result => arg -> result; section_ = term "section"
select_ :: Term eventHandler arg result => arg -> result; select_ = term "select"
small_ :: Term eventHandler arg result => arg -> result; small_ = term "small"
source_ :: Term eventHandler arg result => arg -> result; source_ = term "source"
span_ :: Term eventHandler arg result => arg -> result; span_ = term "span"
strong_ :: Term eventHandler arg result => arg -> result; strong_ = term "strong"
style_ :: Term eventHandler arg result => arg -> result; style_ = term "style"
sub_ :: Term eventHandler arg result => arg -> result; sub_ = term "sub"
summary_ :: Term eventHandler arg result => arg -> result; summary_ = term "summary"
sup_ :: Term eventHandler arg result => arg -> result; sup_ = term "sup"
table_ :: Term eventHandler arg result => arg -> result; table_ = term "table"
tbody_ :: Term eventHandler arg result => arg -> result; tbody_ = term "tbody"
td_ :: Term eventHandler arg result => arg -> result; td_ = term "td"
textarea_ :: Term eventHandler arg result => arg -> result; textarea_ = term "textarea"
tfoot_ :: Term eventHandler arg result => arg -> result; tfoot_ = term "tfoot"
th_ :: Term eventHandler arg result => arg -> result; th_ = term "th"
thead_ :: Term eventHandler arg result => arg -> result; thead_ = term "thead"
time_ :: Term eventHandler arg result => arg -> result; time_ = term "time"
title_ :: Term eventHandler arg result => arg -> result; title_ = term "title"
tr_ :: Term eventHandler arg result => arg -> result; tr_ = term "tr"
track_ :: Term eventHandler arg result => arg -> result; track_ = term "track"
u_ :: Term eventHandler arg result => arg -> result; u_ = term "u"
ul_ :: Term eventHandler arg result => arg -> result; ul_ = term "ul"
var_ :: Term eventHandler arg result => arg -> result; var_ = term "var"
video_ :: Term eventHandler arg result => arg -> result; video_ = term "video"
wbr_ :: Term eventHandler arg result => arg -> result; wbr_ = term "wbr"


-- SVG

circle_ :: Term eventHandler arg result => arg -> result; circle_ = term "circle"
clipPath_ :: Term eventHandler arg result => arg -> result; clipPath_ = term "clipPath"
defs_ :: Term eventHandler arg result => arg -> result; defs_ = term "defs"
ellipse_ :: Term eventHandler arg result => arg -> result; ellipse_ = term "ellipse"
g_ :: Term eventHandler arg result => arg -> result; g_ = term "g"
image_ :: Term eventHandler arg result => arg -> result; image_ = term "image"
line_ :: Term eventHandler arg result => arg -> result; line_ = term "line"
linearGradient_ :: Term eventHandler arg result => arg -> result; linearGradient_ = term "linearGradient"
mask_ :: Term eventHandler arg result => arg -> result; mask_ = term "mask"
path_ :: Term eventHandler arg result => arg -> result; path_ = term "path"
pattern_ :: Term eventHandler arg result => arg -> result; pattern_ = term "pattern"
polygon_ :: Term eventHandler arg result => arg -> result; polygon_ = term "polygon"
polyline_ :: Term eventHandler arg result => arg -> result; polyline_ = term "polyline"
radialGradient_ :: Term eventHandler arg result => arg -> result; radialGradient_ = term "radialGradient"
rect_ :: Term eventHandler arg result => arg -> result; rect_ = term "rect"
stop_ :: Term eventHandler arg result => arg -> result; stop_ = term "stop"
svg_ :: Term eventHandler arg result => arg -> result; svg_ = term "svg"
text_ :: Term eventHandler arg result => arg -> result; text_ = term "text"
tspan_ :: Term eventHandler arg result => arg -> result; tspan_ = term "tspan"
