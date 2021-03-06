
exports = this

# TODO
# ==============================================================================
#
#   - overlay visible in Deck when one panel is not full width/height.
#   - overlay not big enough when one panel is several pages tall
#   - overlay does not cover the content it should cover, slow dat 
#     shit to see what's going on.
#
#   - click to scroll is broken in toc and in the main panel ................ OK
#   - need click on sections to change the adress in the adress bar.
#
#   - Scroll to link borked again ...
#


# Javascript Helpers
# ==============================================================================
type = (item) ->
  Object::toString.call(item)[8...-1].toLowerCase()

String::capitalize = ->
    this.charAt(0).toUpperCase() + this.slice(1)

String::startsWith = (string) ->
    this[...string.length] is string

# Properties
# ==============================================================================
AutoProps = (cls) ->
  prototype = cls.prototype
  properties = {}
  for own name of prototype
    if name.startsWith("get") and name isnt "get"
      propName = name[3].toLowerCase() + name[4..]
      properties[propName] ?= {}
      do (name) ->
        properties[propName].get = -> this[name]()
    if name.startsWith("set") and name isnt "set"
      propName = name[3].toLowerCase() + name[4..]
      properties[propName] ?= {}
      do (name) ->
        properties[propName].set = (args...) -> this[name](args...)
  Object.defineProperties prototype, properties
  return cls

# Method Observers
# ==============================================================================
# 
# Mix the connect method below into your class to enable observers, or
# use the Connect class decorator. 
# Connect to getters / setters to observe properties reads / writes .

Connect = (cls) ->
  prototype = cls.prototype
  prototype.connect = connect
  return cls

connect = (links) ->
  all_targets = this._targets ?= {}
  for name, target of links
    targets = all_targets[name] ?= []
    targets.push target
    method = this[name]
    if not this[name]._connected?
      do (name, method) =>
        new_method = (args...) =>
          output = method.call(this, args...)
          for _target in this._targets[name]
            _target(args...)
          return output 
        new_method._connected = on
        this[name] = new_method
  return this

# Component Model
# ==============================================================================
#   
# Basics:
#
#   - Components represent (arrays of) HTML elements, logic and data.
#     Think of it as a (lightweight) extension of the jQuery object.
# 
#   - Use the HTML factories to create components:
#
#         a = HTML.a href: "http://coffeescript.org/" 
#
#         header = HTML.header(HTML.h1 "Title", HTML.h2 "Subtitle")
#
#         p = HTML.p "I can't feel my legs"
#
#   - The `$` attribute enables the use of components with jQuery:
#
#         $("body").prepend header.$
#
#     Use the "co" data stored in the jQuery object to get the component back:
#
#         > header.$.data("co") is header
#         true
#
#     JQuery objects can be used directly in HTML factories.         
#
#   - Custom components encapsulate logic and data:
#
#         switch = HTML.SwitchButtonButton() 
#         switch.toggle(on)
#         console.log "Turned on ?", switch.status
#
# TODO:
#
#   - embed a reference to the component inside the jQuery object.
#     Use data attributes. What name ? (Short would be nice ... "co" ?)
#     Yeah, let's try "co" for a while ...
#
#   - document how most component accept custom constructor arguments AND
#     usually some (possibly filtered) native ones. Non-applicable options
#     should be removed before being transmitted up in the object constructors. 
#     Describe how to handle nesting with custom components ? 
#     (Still fuzzy in my head)
#
#   - mix `connect` in Component by default ? Yes. Document usage (ex: button)
#

# define "native" components (wrapper for HTML elements ? with a .$ attribute) ?
# can we create classes programatically ?

# TODO:
#   
#   - native HTML Element vs Component ? Name of the base ? Element, then
#     Native / Custom or all inherit from Element ? Have a look at the 
#     terminology around Web Components / Shadow DOM, etc. Maybe don't
#     worry too much for now about the native/custom distinction, inherit
#     from an abstract Element.
#
#   - create bone fide classes for all Elements
#
#   - all Elements shall allow for new-less instantiation.
#
#   - update: don't bind the jQuery or DOM object to the Element by default.
#     do it for CustomElements ? (and use the "elt" datum ?"

HTML = {}

do ->
  HTML.Element = class Element
    constructor: (elt) ->
      if not (this instanceof HTML.Element)
        return new HTML.Element(elt)

      if elt instanceof HTML.Element
        return elt
      else if type(elt) is "string"
        return HTML.Element($(document.createTextNode(elt)))
      else if elt instanceof jQuery
        this.$ = elt
        return this
      else if elt?.tagName? # DOM node
        return HTML.Element $(elt)
      else
        error  = "HTML.Element(elt) -- invalid element of type #{type(elt)}:\n"
        error += "#{elt}"
        throw error

    Element.shift = (attributes, children) ->
      attributes ?= {}
      if type(attributes) isnt "object" or 
      (attributes instanceof jQuery) or 
      (attributes.$ instanceof jQuery) 
        # not attributes, but content
        children.unshift(attributes)
        attributes = {}
      [attributes, children]
      
  tags = 
    """
    a abbr address area article aside audio b base bdi bdo blockquote body br
    button canvas caption cite code col colgroup datalist dd del details dfn
    dialog div dl dt em embed fieldset figcaption figure footer form h1 h2 h3
    h4 h5 h6 head header hr html i iframe image img input ins kbd keygen label
    legend li link main map mark menu menuitem meta meter nav noscript object
    ol optgroup option output p param pre progress q rp rt ruby s samp script
    section select small source span strong style sub summary sup table tbody
    td textarea tfoot th thead time title tr track u ul var video wbr
    """.split(/\s/)

  for tag in tags
    do (tag) ->
      HTML[tag] = cls = class extends HTML.Element
        constructor: (attributes, children...) ->
           if not (this instanceof cls)
             return new cls(attributes, children...)
           [attributes, children] = HTML.Element.shift(attributes, children)
           children = (HTML.Element(child) for child in children)
           this.$ = $("<#{tag}></#{tag}>", attributes)
           this.$.append(child.$) for child in children

  # Support args shift & automatic promotion in all custom Elements ?
  HTML.CustomElement = Connect class CustomElement extends HTML.Element
    constructor: ->
      if this.constructor is CustomElement
        throw "CustomElement class is abstract"

#class Component
#  # Remark: not sure a constructor is the right pattern here 
#  # (given the extra tag argument).
#  constructor: (tag, attributes, children...) ->
#      [attributes, children] = Component.normalize(tag, attributes, children)
#      this.$ = $("<#{tag}></#{tag}>", attributes)
#      this.$.append(child) for child in children
#      this.$.data "co", this

#  # I have conflated two things here: the "unshift" of attributes
#  # and the automatic cast of children to jQuery. Split those methods.
#  # Should I cast the children to component instead ? What method to
#  # wrapp a jQuery object as a component ?
#  this.normalize = (tag, attributes, children) ->
#    attributes ?= {}
#    if type(attributes) in ["array", "string"] or 
#       (attributes instanceof jQuery) or 
#       (attributes.$ instanceof jQuery) 
#      # not attributes, but content
#      children.unshift(attributes)
#      attributes = {}

#    new_children = []
#    for child in children
#      if type(child) is "string" # Text Node
#        child = $(document.createTextNode(child))
#      else if child?.$ instanceof jQuery # Component
#        child = child.$
#      else if child?.tagName? # DOM Node
#        child = $(child)
#      # TODO: accept arrays of DOM Nodes ?
#      else if not (child instanceof jQuery)
#        throw "invalid child."
#      new_children.push child
#    return [attributes, new_children]

#  connect: connect


## HTML Builder
## Drop the HTML ? Use a Factory instead ? Can I do that in cs (as a decorator ?)
## Is is wise to keep only the factory (it means that I cannot typecheck the 
## instances ...). Is react using the same namespace for native & custom ?
## OK, we can do it, I am just not sure that we should.
## Wrt typecheck, we could add a name attribute to the function (the cls.name)
## but how would we do the typecheck normally ? Consider instanceof stuff.
#HTML = (cls) -> # add Component classes as factories the HTML namespace.
#  HTML[cls.name] = (args...) -> new cls(args...)
#  return cls

## TODO: find and exhasutive list of HTML tags in plain text.
##       Nota: this stuff is nice but we cannot typecheck against the tag name
##       can we ?
#for tag in "a aside body code div em h1 h2 h3 h4 h5 head html i main nav p pre span strong style".split(" ")
#  HTML[tag] = do (tag) ->
#    (args...) ->
#      (new Component(tag, args...)).$


# Style Helpers
# ==============================================================================

class Style
  this.reset = ->
    style = HTML.style type:"text/css",
      """
      /* http://meyerweb.com/eric/tools/css/reset/ 
         v2.0 | 20110126
         License: none (public domain)
      */

      html, body, div, span, applet, object, iframe,
      h1, h2, h3, h4, h5, h6, p, blockquote, pre,
      a, abbr, acronym, address, big, cite, code,
      del, dfn, em, img, ins, kbd, q, s, samp,
      small, strike, strong, sub, sup, tt, var,
      b, u, i, center,
      dl, dt, dd, ul, ol, li,
      fieldset, form, label, legend,
      table, caption, tbody, tfoot, thead, tr, th, td,
      article, aside, canvas, details, embed, 
      figure, figcaption, footer, header, hgroup, 
      menu, nav, output, ruby, section, summary,
      time, mark, audio, video {
	      margin: 0;
	      padding: 0;
	      border: 0;
	      font-size: 100%;
	      font: inherit;
	      vertical-align: baseline;
      }
      /* HTML5 display-role reset for older browsers */
      article, aside, details, figcaption, figure, 
      footer, header, hgroup, menu, nav, section {
	      display: block;
      }
      body {
	      line-height: 1;
      }
      ol, ul {
	      list-style: none;
      }
      blockquote, q {
	      quotes: none;
      }
      blockquote:before, blockquote:after,
      q:before, q:after {
	      content: '';
	      content: none;
      }
      table {
	      border-collapse: collapse;
	      border-spacing: 0;
      }
      """
    $("html head").prepend style.$

  this.apply = (styleSheet, context) ->
    styleSheet ?= {}
    if type(styleSheet) is "array"
      for _styleSheet in styleSheet
        Style.apply _styleSheet, context
      return undefined

    for selector, assignments of styleSheet
      if not context?
        subcontext = $(selector)
      else
        subcontext = context.find(selector)
      for name, value of assignments
        if type(value) in ["number", "string"]
          subcontext.css(name, value)
        else if type(value) is "object"
          style = {}
          style[name] = value
          Style.apply style, subcontext
        else
          throw "Style.apply -- invalid #{property} value: #{value}"
    return undefined

# Typography
# ==============================================================================

styleText = ->
  fontSize =
    small: "14px"
    medium: "22px"
    large: "28px"
    xLarge: "36px"
    huge: "48px"

  Style.apply [
    body:
      fontFamily: "Alegreya, serif"
      fontSize: fontSize.medium
      fontWeight: "normal"
      lineHeight: 1.5
      textAlign: "justify"
      textRendering: "optimizeLegibility"
      hyphens: "auto"
      MozHyphens: "auto"
    em:
      fontStyle: "italic"
    strong:
      fontWeight: "bold"
    "h1, h2, h3":
      margin: "0.5em 0em 0.5em 0em"
    h1:
      fontSize: fontSize.xLarge
      fontWeight: "bold"
    h2:
      fontSize: fontSize.large
      fontWeight: "bold"
    h3:
      fontSize: fontSize.medium
      fontWeight: "bold"
    "h4, h5, h6":
      fontSize: fontSize.medium
      fontWeight: "bold"
      float: "left"
      marginRight: "1em"
      marginTop: "0em"
      marginLeft: "0em"
      marginBottom: "0em"
    p:
      marginTop: "1em"
      marginBottom: "1em"
    sup:
      verticalAlign: "super"
      fontSize: fontSize.small
  ,
    "main header":
      h1:
        fontSize: fontSize.huge
        fontWeight: "bold"
      ".author":
        fontSize: fontSize.xLarge
        fontWeight: "bold"
  ]

  Style.apply
    main:
      ul:
        listStyleType: "disc"
        li: marginLeft: "2em"

  Style.apply
    table:
      borderTopStyle: "solid"
      borderBottomStyle: "solid"
      borderWidth: "2px"
      marginLeft: "auto"
      marginRight: "auto"
      borderCollapse: "collapse"
      "td:not(:last-child), th:not(:last-child)":
        paddingRight: "1em"
      "tr.header":
        borderWidth: "1.5px"
        borderBottomStyle: "solid"

  Style.apply
    code:
      fontSize: "20.5px"
      fontFamily: "Inconsolata"
    pre:
      color: "#000000"
      backgroundColor: "#ebebeb"
      marginTop: "1em"
      marginBottom: "1em"
      padding: "0.5em 1em 0.5em 1em"



# Icons
# ==============================================================================
# TODO: implement all font awesome features from 
#       <https://fortawesome.github.io/Font-Awesome/examples/> 
#       Still TODO:
#         - bordered / pulled
#         - rotated / flipped
#         - stacked.

# A declarative version of properties (with defaults) would be nice to have,
# to avoid most of the boilerplate in the constructor. We could reduce it by
# 90% ...Have a look at React props ?
HTML.Icon = AutoProps class Icon extends HTML.CustomElement
  constructor: (options) ->
    if not (this instanceof HTML.Icon)
      return new HTML.Icon(options)
    if not options.type?
      throw "undefined icon type"
    else
      this._type = options.type
      delete options.type

    this._size = options.size
    delete options.size
    this._size ?= 1.0

    extra = "fixedWidth li pulse spin".split(" ")
    for property in extra
        this["_#{property}"] = options[property]
        delete options[property]
        this["_#{property}"] ?= false

    # factor out this pattern ? ("customize / wrap" ?)
    this.$ = HTML.i(options).$
    this.$.data "elt", this

    this.$.addClass "fa"
    # Synchronize DOM with state
    this.type = this._type
    this.size = this._size
    for property in extra
       this[property] = this["_#{property}"]

  # Set up basic setters & getters
  faTags =  
    fixedWidth: "fw"
    list: "li"
    pulse: "pulse"
    spin: "spin"

  for name, tag of faTags
    do (name, tag) =>
      capName = name.capitalize()
      this::["get" + capName] = ->
        this["_#{name}"]
      this::["set" + capName] = (status) ->
        if status
          this.$.addClass "fa-#{tag}"
        else
          this.$.removeClass "fa-#{tag}"

  # Other setters & getters
  getType: -> 
    this._type

  setType: (type) -> 
    this.$.removeClass "fa-#{this._type}" if this._type?
    this.$.addClass "fa-#{type}"
    this._type = type

  Icon.sizes = [1.0, 4/3, 2.0, 3.0, 4.0, 5.0]

  getSize: -> 
    this._size

  setSize: (size) ->
    if size in Icon.sizes
      this._size = size
      this.$.removeClass "fa-lg fa-2x fa-3x fa-4x fa-5x"
      switch size
        when 4/3
          this.$.addClass "fa-lg"
        else
          this.$.addClass "fa-#{size}x"
    else
      sizes = Icon.sizes.join ", "
      error = "invalid icon size: #{size} is not in [#{sizes}]" 
      throw error
  

HTML.MathJaxLoader = class MathJaxLoader extends HTML.CustomElement
  constructor: (options) ->
    if not (this instanceof HTML.MathJaxLoader)
      return new HTML.MathJaxLoader(options)
    options.type = "cog"
    this.icon = HTML.Icon(options)
    this.$ = this.icon.$
    this.$.data "elt", this

    this.$.addClass "mathjax-loader"
    this.icon.spin = true
    MathJax.Hub.Register.StartupHook "End", => 
      this.icon.spin = false
      this.$.css display: "none"
      this.done()

  done: ->

HTML.SwitchButton = class SwitchButton extends HTML.CustomElement
  constructor: (options) ->
    if not (this instanceof HTML.SwitchButton)
      return new HTML.SwitchButton(options)
    {@typeOn, @typeOff} = options or {}
    delete options.typeOn
    delete options.typeOff
    this.typeOn ?= "toggle-on"
    this.typeOff ?= "toggle-off"
    options.css ?= {}
    options.css.cursor ?= "pointer"

    this.icon = HTML.Icon(type: this.typeOff)
    this.$ = HTML.div(options, this.icon).$
    this.$.data "elt", this

    this.off()
    this.$.on "click": => this.toggle()

  # consider using a single "state" (or status) property instead of "on" and 
  # "off" methods ? (Actually we could keep on and off as convenience methods).
  on: =>
    this.icon.type = this.typeOn
    this.status = on

  off: =>
    this.icon.type = this.typeOff
    this.status = off

  toggle: (status = undefined) =>
    status ?= not this.status
    if status is on 
      this.on() 
    else 
      this.off()

# Code Block
# ==============================================================================
#
# TODO:
#
#   - basically, some text content inside a pre + code environment.
#     code is here to mean "monospace", "pre" to keep whitespace ok.
#   
#   - deal with dynamic construction of code blocks from js and
#     "recycling" from existing pre + code components.
#
#   - (fenced) code blocks from pandoc can be annotated with some
#     specific attributes, including the language name. 
#     AFAICT, they are:
#
#       - the language (fixed list)
#
#       - numberLines / startFrom
#      
#   - pandoc may generate a more complex structure with a div/table/pre/code
#     structure ... that depends on the attributes that are given ... 
#     The code itself may be mangled with various tags (for the highlighter ?)
#     Is pandoc fucking PARSING the language ? Yeah actually ...
#     I may have to disable all this stuff ... there are some experiments
#     that shall be done here.
#
#   - deal with the language specification (see e.g. 
#     <http://stackoverflow.com/questions/5134242/semantics-standards-and-using-the-lang-attribute-for-source-code-in-markup>
#   
#    - I want the original text, or some modified version of it, accessible
#      for clipboarding. An example of modification: if the text is a sequence
#      of bash commands, or python commands in the interpreter (and the results),
#      I probably only want the commands, not the prompt or the results.
#
#    - icon top right on hover and opacification of the code for clipboard ?
#
#    Do some experiments on code creation first, then see how to connect this
#    to pandoc output.
#
# Use highlight.js automaticde detection ?
#
HTML.CodeBlock = class CodeBlock extends HTML.CustomElement
  constructor: (options) ->
    if not (this instanceof HTML.CodeBlock)
      return new HTML.CodeBlock(options)
    options ?= {}
    this.text = options.text # "text" really ? Calling like a HTML.code would be nicer ...
    # that means that we need the shift / promote dance used by native Elements.
    this.text ?= ""
    delete options.text

    options.css ?= {}
    options.css.cursor = "pointer"

    # well, the button should use a Button Element (TODO) ...
    this.$ = (HTML.div options, HTML.pre HTML.code(this.text)).$

    this.clipboard = new Clipboard this.$[0],
        text: (trigger) => this.text


# Deck
# ------------------------------------------------------------------------------

# TODO: if the transition is aborted, focus is not given to the proper card.
HTML.Deck = AutoProps class Deck extends HTML.CustomElement
  constructor: (attributes, children...) ->
    if not (this instanceof HTML.Deck)
      return new HTML.Deck(attributes, children...)
    [attributes, children] = HTML.Element.shift(attributes, children)
    children = (HTML.Element(child) for child in children)

    this.$ = HTML.div(attributes).$

    css = ->
      width: "100%", height:"100%",
      padding: "0px"
      overflowY: "scroll"
      overflowX: "hidden" 
      position: "absolute",
      transition: "transform 1.2s cubic-bezier(0.0,0,0.25,1)"

    cards = for child in children
      HTML.div
         css: css()
         attr: tabindex: -1
         child.$

    this.$.append (HTML.div
        class: "cards"
        css:
          position: "relative"
          width: "100%", height: "100%"
          overflowX: "hidden"
          overflowY: "hidden"
        cards...).$

    this.cards = this.$.find(".cards").children()
    this.index = 0

  resetScroll: () ->
    cardHolder = this.$.find(".cards")
    cardHolder.scrollLeft(0)

  getIndex: -> this._index

  setIndex: (index) ->
    index = Math.max(0, Math.min(this.cards.length - 1, index))
    this._index = index
    this.cards.each (i) ->
      if i < index
        $(this).css transform: "translateX(-100%)", zIndex: -1
      else if i is index
        $(this).css transform: "translateX(0%)", zIndex: 0
        $(this).on "transitionend", ->
          $(this).focus()
          $(this).off "transitionend"
      else if i > index
        $(this).css transform: "translateX(100%)", zIndex: -1



# Table of Contents
# ------------------------------------------------------------------------------


# TODO: update the style (background) of the active section, maybe autofocus.
#       Think of the API for the active entry ... use the target (href) ?
HTML.TOC = AutoProps class TOC extends HTML.CustomElement
  constructor: (options) ->
    if not (this instanceof HTML.TOC)
      return new HTML.TOC(options)
    options ?= {}
    root = options.root
    if not root?
      throw "TOC: undefined root"
    delete options.root
    options.css ?= {} 
    options.css.overflow = "hidden"

    div = HTML.div options, 
      HTML.h1 "Contents"
      HTML.nav TOC.outline(root)
    this.$ = div.$

    this.style()
    this.show()

  style : =>

    toc = this
    this.$.find("li").css
      marginLeft: "0px" 
      listStyleType: "none"

    anchors = this.$.find("a")
    anchors.css
      textDecoration: "none"
      display: "block"
    anchors.mouseenter -> $(this).css backgroundColor: "#a0a0a0"
    anchors.mouseleave -> 
      if $(this).attr("href") is toc.active 
        $(this).css backgroundColor: "white" 
      else
        $(this).css backgroundColor: ""

    indent = (root, level) =>
      root ?= this.$.find("ul").first()
      level ?= 0
      isTree = (elt) -> (elt.children().first().prop("tagName") is "UL")
      root.children("li").each (index, elt) ->
        if isTree $(elt)
          ul = $(elt).children().first()
          indent ul, (level + 1)
        else
          $(elt).children().first().css paddingLeft: (0.5 + 1.5 * level) + "em"

    indent() 

#  # need to hook that to link click :(
#  focus: => # find the active section, change the style accordingly
#    hash = window.location.hash
#    anchor = this.$.find("a[href='#{hash}']")
#    anchor.css backgroundColor: "white" 
#    # this is going to be overwritten at the first occasion do something else.

  getActive: -> this._active

  setActive: (hash) ->
    anchor = this.$.find("a[href='#{hash}']")
    console.log anchor[0].outerHTML
    if this._active?
      anchor = this.$.find("a[href='#{this._active}']")
      anchor.css backgroundColor: ""
    this._active = hash
    anchor = this.$.find("a[href='#{hash}']")
    console.log "active anchor", anchor
    anchor.css backgroundColor: "white"  
    console.log anchor.css "backgroundColor"
    console.log anchor[0].outerHTML
      
  show: =>
    this.$.css display: "block"

  hide: =>
    this.$.css display: "none"

  # Remark: we rely ONLY on section tags for nesting, not headings levels.
  this.outline = (root) ->
    list = HTML.ul()
    root.$.children().each ->
      tag = this.nodeName.toLowerCase()
      if tag in "h1 h2 h3 h4 h5 h6".split(" ")
        text = $(this).html()
        list.$.append $("<li>#{text}</li>") if text
      else if tag is "header"
        outline = TOC.outline HTML.Element(this)
        list.$.append outline.$.children().first()
        list.$.append outline.$.children().clone()
      else if tag is "div"
        outline = TOC.outline HTML.Element(this)
        list.append outline.$.children() if outline.$.children().length > 0
      else if tag is "section"
        outline = TOC.outline HTML.Element(this)
        list.$.append HTML.li(outline).$ if outline.$.children().length > 0
    return list


toc_switch = (status=undefined) ->
  overlay = $(".overlay")
  status ?= overlay.css("display") is "none"
  if status == on
    overlay.css("display", "block")
    overlay.focus()
    $("body").css(overflow: "hidden")
    $("#hamburger").removeClass("fa-bars").addClass("fa-times")
  else
    overlay.css("display", "none")
    $("body").css(overflow: "auto")
    $("#hamburger").removeClass("fa-times").addClass("fa-bars")


setupTOC = () ->
  # Transfer the body contents to main
  main = HTML.main 
    css:
      margin: "auto"
      maxWidth: "32em"
      width: "auto"
      height: "auto" 
    $("body").children()

  # Add the hamburger button.
  topLeftCorner = 
    position: "fixed", 
    top: "1em", 
    left: "1em", zIndex: 999
  hamburger = HTML.SwitchButton css: topLeftCorner, typeOff: "bars", typeOn: "times"
  $("body").prepend hamburger.$

  # Create the table of contents
  window.toc = toc = HTML.TOC
    root: 
      main # declare as a child instead ? Yes, more consistent.
    css:
      minHeight: "100vh"
      paddingLeft: "5em"
      backgroundColor: "#f0f0f0"

  # Setup the TOC / content panel and wire with the button
  window.deck = deck = HTML.Deck
    css:
      width:"100vw" 
      height:"100vh" 
      overflowX: "hidden" 
      overflowY: "hidden" 
    toc
    main

  $("body").append deck.$

  # table of contents: close on click
  $("a", toc.$).on "click", -> hamburger.off()

  hamburger.connect
    on: -> deck.index = 0
    off: -> deck.index = 1

#  hamburger.connect # WTF ? toc.focus is not a function ?
#    on: -> toc.focus() # hook in a better place (refactor anchor click callbacks)

  hamburger.off()



# Source: <http://www.adriantomic.se/development/jquery-localscroll-tutorial/>
# TODO: make an instantaneous scroll when the link is from the toc ?
manageLinks = ->  
  $("a[href*=#]:not([href=#])").click (event) ->
    if location.pathname.replace(/^\//,'') is this.pathname.replace(/^\//,'') and
       location.hostname is this.hostname

      # need to reach a holder in the hierarchy that has a scrollbar
      holder = $("main").parent()
      if not (holder.css("overflowY") in ["scroll", "auto"])
        throw "main holder has no scroll"

      if $.contains(window.toc.$[0], this)
        duration = 0
      else
        duration = 600

      target = $(this.hash)
      holder.animate
        scrollTop: holder.scrollTop() + target.offset().top
      ,    
        duration: duration, 
        easing: "swing"

      # BUG: setting the location hash (directly or via pushState) "focuses"
      #      on the target ... including horizontally somehow, which is a pain
      #      in the ass ... Maybe I could set the lateral scroll just after
      #      the location change just to invert the move ? Or "defuse" the id.
      hash = this.hash
      target = $(hash)
      target.attr id: "__" + hash[1...] + "__"
      empty = (HTML.div
        id: hash[1...]
        css:
          position: "fixed"
          visibility: "hidden"
      ).$
      $("body").append empty
      location.hash = hash # Aah, nowhere to go now ! Shit, the bastard 
      # remembers the previous location ...
      empty.remove()
      target.attr id: hash[1...]
      
      #baseUrl = location.toString()[...location.hash.length]
      #window.history.pushState {}, "", baseUrl + this.hash

      window.toc.active = window.location.hash

      return false

$ ->

  Style.reset()

  code = 
    """
    $ git clone whatever
    """

  # highlightAuto sucks too much ; maybe I could hack it and only get
  # a good candidate within a language list (sy: bash, python, js, cs ?)
  console.log window.hljs.highlightAuto code, ["Bash", "Python", "CoffeeScript", "JavaScript"]


  body = $("body")
  html = $("html")
  body.css 
    overflow: "hidden"
  html.css overflow: "hidden"

  code = """
  if True:
    print 42
  else:
    print 43

  print "DONE."
  """

  #body.prepend HTML.CodeBlock(text: code).$

  setupTOC()

  middleLeft = 
    css: {position: "fixed", top: "50%", left: "1em"}
  loader = HTML.MathJaxLoader(middleLeft)
  loader.connect # prevent Mathjax loader to refocus on the math content.
    done: ->
      dontYouFuckDareToMove = window.setInterval (-> window.deck.resetScroll()), 10
      window.setTimeout (-> window.clearInterval dontYouFuckDareToMove), 1000
  body.prepend loader.$


  manageLinks()
  
  styleText()

  #testAnim()

  $(document).on "keydown", (event) ->
    switch event.which
      when "S".charCodeAt(0)
        if not body.hasClass "slides"
          body.addClass "slides"
        else
          body.removeClass "slides"
      when "O".charCodeAt(0)
        if not body.hasClass "outline"
          switchOutline on
          body.addClass "outline"
        else
          switchOutline off
          body.removeClass "outline"
      when "H".charCodeAt(0)
        highlight()


