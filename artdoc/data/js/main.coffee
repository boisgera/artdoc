
# TODO
# ==============================================================================
#
#   - overlay visible in panels when one panel is not full width/height.
#   - overlay not big enough when one panel is several pages tall
#   - overlay does not cover the content it should cover, slow dat 
#     shit to see what's going on.
#
#   - click to scroll is broken in toc and in the main panel ................ OK
#   - need click on sections to change the adress in the adress bar.
#
#   - Scroll to link borked again ...
#

# CSS Reset
# ==============================================================================
reset = ->
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
    dl, dt, dd, ol, ul, li,
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
  $("html head").append style


# Typography
# ==============================================================================

# TODO: transform everything here.

styleText = ->
  fontSize =
    small: "14px"
    medium: "22px"
    large: "28px"
    xLarge: "36px"
    huge: "48px"

  $("body").css
    fontFamily: "Alegreya, serif"
    fontSize: fontSize.medium
    fontWeight: "normal"
    lineHeight: 1.5
    textAlign: "justify"
    textRendering: "optimizeLegibility"
    hyphens: "auto"
    MozHyphens: "auto"

  $("h1, h2, h3").css
    margin: "0.5em 0em 0.5em 0em"

  $("h1").css
    fontSize: fontSize.xLarge
    fontWeight: "bold"

  $("h2").css
    fontSize: fontSize.large
    fontWeight: "bold"

  $("h3").css
    fontSize: fontSize.medium
    fontWeight: "bold"

  $("h4, h5, h6").css
      fontSize: fontSize.medium
      fontWeight: "bold"
      float: "left"
      marginRight: "1em"
      marginTop: "0em"
      marginLeft: "0em"
      marginBottom: "0em"

  $("h1", "main header").css 
    fontSize: fontSize.huge
    fontWeight: "bold"

  $(".author", "main header").css
    fontSize: fontSize.xLarge
    fontWeight: "bold"

  $("p").css
    marginTop: "1em"
    marginBottom: "1em"

  return undefined

# Javascript Helpers
# ==============================================================================
type = (item) ->
  Object::toString.call(item)[8...-1].toLowerCase()

# Component Model
# ==============================================================================
#
# Should be "similar" in usage to native HTML.div, HTML.p, etc. factories that
# return jQuery objects:
#   - the classes may declare additional, stateful methods (show, click, etc.)
#   - classes are added to the HTML namespace as factories (no new).
#   - they have a "jQuery" or "elt(s)" method or attribute (?) that gives the content.
#     (html is already used) (think of static vs dynamic issue).
#   - the HTML builder recognizes such components and automatically converts
#     them to jquery,
#   - when they require jquery arguments in their constructor, the components
#     should also perform automatically this conversion. They don't have to
#     keep the same options, children constructor conventions ... but it could
#     be handy, the options to children stuff should therefore be normalized.
#     See for example TOC: it needs a root, that does not fit in the model
#     (unless we put that in the options and remove the root field before the
#     processing by the ancestor ? makes sense).

class Component

  # not 100% sure a constructor is the right pattern here (given the extra tag
  # argument).
  constructor: (tag, attributes, children...) ->
      [attributes, children] = Component.normalize(tag, attributes, children)
      this.$ = $("<#{tag}></#{tag}>", attributes)
      this.$.append(child) for child in children

  this.normalize = (tag, attributes, children) ->
    attributes ?= {}
    if type(attributes) in ["array", "string"] or 
       (attributes instanceof jQuery) or 
       (attributes.$ instanceof jQuery) 
      # not attributes, but content
      children.unshift(attributes)
      attributes = {}

    new_children = []
    for child in children
      if type(child) is "string" # Text Node
        child = $(document.createTextNode(child))
      else if child?.$ instanceof jQuery # Component
        child = child.$
      else if child?.tagName? # DOM Node
        child = $(child)
      # TODO: accept arrays of DOM Nodes ?
      else if not (child instanceof jQuery)
        throw "invalid child."
      new_children.push child
    return [attributes, new_children]


# HTML Builder
# ==============================================================================
HTML = (cls) -> # add Component classes as factories the HTML namespace.
  HTML[cls.name] = (args...) -> new cls(args...)

for tag in "a aside body div em h1 h2 h3 h4 h5 head html i main nav p span strong style".split(" ")
  HTML[tag] = do (tag) ->
    (args...) ->
      (new Component(tag, args...)).$
 
#  HTML[type_] = do (type_) -> 
#    (attributes, children...) ->
#      attributes ?= {}
#      if type(attributes) in ["array", "string"] or (attributes instanceof jQuery) or (attributes.html instanceof jQuery) 
#        # not attributes, but content
#        children.unshift(attributes)
#        attributes = {}
#      elt = $("<#{type_}></#{type_}>", attributes)
#      for child in children
#        if type(child) is "string"
#          child = document.createTextNode(child)
#        else if child.html instanceof jQuery
#          child = child.html
#        elt.append child
#      return elt


# Method Observers
# ==============================================================================
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
            _target(this, args...)
          return output 
        new_method._connected = on
        this[name] = new_method
  return this

# Icons
# ==============================================================================
extend = (base, diff) ->
  for k, v of diff
    bv = base[k]
    if k is "css" and bv?
      extend(bv, v)
    else if k is "class" and bv?
      base[k] = [base[k], v].join(" ")
    else
      base[k] = v
  return base

configure = (html, options) ->
  options ?= {}
  html.attr("id", options.id) if options.id?
  html.addClass(options.class) if options.class?
  html.css(options.css) if options.css?  

HTML class Icon extends Component
  constructor: (options) ->
    iconType = options.type
    if not iconType?
      throw "undefined icon type"
    options.type = undefined
    options.class = (options.class or "") + " fa fa-#{iconType}"
    options.fontSize ?= "1em"
    super "i", options

  spin: (status = on) ->
    if status
      this.$.addClass "fa-spin"
    else
      this.$.removeClass "fa-spin"

HTML class MathJaxLoader extends Icon
  constructor: (options) ->
    options.type = "cog"
    options.class = (options.class or "") + " mathjax-loader"
    super options
    this.spin()
    MathJax.Hub.Register.StartupHook "End", => this.$.remove()

  debug: ->
    MathJax.Hub.signal.Interest (msg) -> 
      console.log "MathJax:", msg[0]

# Deck / Panels
# ------------------------------------------------------------------------------
HTML class Panels extends Component # TODO: rename "Deck" ? # Shit, needs some jQuery args, no promotion.
  constructor: (options, items...) ->

    super "div", options

    overlay = HTML.div
      class: "overlay"
      css:
        width: "100%", height:"100%", position: "absolute"
        backgroundColor: "#ffffff"
        opacity: 0.8
        zIndex: -1
        pointerEvents: "none"

    css = ->
      width: "100%", height:"100%",
      padding: "0px"
      overflowY: "scroll"
      overflowX: "hidden"
      position: "absolute",
      transition: "transform 1.2s cubic-bezier(0.0,0,0.25,1)"
    divs = []

    for item, i in items
      divs.push HTML.div(css: css(), item)

    this.$.append HTML.div
        css:
          position: "relative"
          width: "100%", height: "100%"
          overflowX: "hidden"
          overflowY: "hidden"
        overlay
        divs...

    this.inner = this.$.children()
    this.items = this.inner.children()

    this.setIndex 0

  setIndex: (index) =>
    index = Math.max(0, Math.min(this.items.length-2, index))
    this.index = index
    this.items.each (i) ->
      if i is 0
        undefined # this is the semi-transparent overlay
      else if i-1 < index
        $(this).css transform: "translateX(-100%)", zIndex: -2
      else if i-1 is index
        $(this).css transform: "translateX(0%)", zIndex:0
        $(this).on "transitionend", -> 
          #if $(this).css("transform") is "matrix(1, 0, 0, 1, 0, 0)"
          #  undefined
          $(this).off "transitionend"
      else
        $(this).css transform: "translateX(100%)", zIndex: -2

# Switch Button
# ------------------------------------------------------------------------------
HTML class Switch extends Component
  constructor: (options) ->
    this.typeOn = options.typeOn
    this.typeOff = options.typeOff
    options.typeOn = options.typeOff = undefined
    options.class = (options.class or "") + " switch fa"
    options.css = (options.css or {})
    options.css.cursor = "pointer"
    options.css.fontSize = "1em"

    super "i", options

    this.off()
    this.$.on "click": => this.toggle()

  connect: connect

  on: =>
    this.$.removeClass("fa-" + this.typeOff).addClass("fa-" + this.typeOn)
    this.status = on

  off: =>
    this.$.removeClass("fa-" + this.typeOn).addClass("fa-" + this.typeOff)
    this.status = off

  toggle: (status = undefined) =>
    status ?= not this.status
    if status is on 
      this.on() 
    else 
      this.off()

# Table of Contents
# ------------------------------------------------------------------------------

# TODO: update the style (background) of the active section, maybe autofocus.

HTML class TOC extends Component
  constructor: (options) ->
    root = options.root
    options.root = undefined

    super "div", options

    this.$.css overflow: "hidden"
    this.$.append HTML.h1 "Contents"
    this.$.append this.nav = HTML.nav (TOC.outline(root))

    this.style()
    this.show()

  style : =>
    $("li", this.nav).css 
      listStyleType: "none"
      margin: "0px"

    anchors = $("a", this.nav)
    anchors.css display: "block"
    anchors.mouseenter -> $(this).css backgroundColor: "#a0a0a0"
    anchors.mouseleave -> $(this).css backgroundColor: ""

    indent = (root, level) =>
      root ?= this.nav.find("ul").first()
      level ?= 0
      isTree = (elt) -> (elt.children().first().prop("tagName") is "UL")
      root.children("li").each (index, elt) ->
        if isTree $(elt)
          ul = $(elt).children().first()
          indent ul, (level + 1)
        else
          $(elt).children().first().css paddingLeft: 1.5 * level + "em"

    indent()

    $("a", this.nav).css
      textDecoration: "none"       

  # need to hook that to link click :(
  focus: => # find the active section, change the style accordingly
    hash = window.location.hash
    anchor = this.nav.find("a[href='#{hash}']")
    anchor.css backgroundColor: "white" 
    # this is going to be overwritten at the first occasion do something else.

  show: =>
    this.$.css display: "block"

  hide: =>
    this.$.css display: "none"

  # Remark: we rely ONLY on section tags for nesting, not headings levels.
  this.outline = (root = undefined) ->
    root ?= $("body")    
    list = $("<ul></ul>")
    root.children().each ->
      tag = this.nodeName.toLowerCase()
      if tag in "h1 h2 h3 h4 h5 h6".split(" ")
        text = $(this).html()
        list.append $("<li>#{text}</li>") if text
      else if tag is "header"
        outline = TOC.outline $(this)
        list.append outline.children().first()
        list.append outline.children().clone()
      else if tag is "div"
        outline = TOC.outline $(this)
        list.append outline.children() if outline.children().length > 0
      else if tag is "section"
        outline = TOC.outline $(this)
        list.append $("<li></li>").append(outline) if outline.children().length > 0
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
  hamburger = HTML.Switch typeOff: "bars", typeOn: "times", css: topLeftCorner
  $("body").prepend hamburger.$

  # Create the table of contents
  toc = HTML.TOC
    root: 
      main
    css: 
      paddingLeft: "5em"
      backgroundColor: "#f0f0f0"

  # Setup the TOC / content panel and wire with the button
  panel = HTML.Panels
    css:
      width:"100vw" 
      height:"100vh" 
      overflowX: "hidden", 
      overflowY: "hidden", 
    toc,
    main

  $("body").append panel.$

  # table of contents: close on click
  $("a", toc.$).on "click", -> hamburger.off()

  hamburger.connect
    on: -> panel.setIndex 0
    off: -> panel.setIndex 1

  hamburger.connect 
    on: -> toc.focus() # hook in a better place (refactor anchor click callbacks)

  hamburger.off()



#<http://www.adriantomic.se/development/jquery-localscroll-tutorial/>
manageLinks = ->  
  $("a[href*=#]:not([href=#])").click (event) ->
    if location.pathname.replace(/^\//,'') is this.pathname.replace(/^\//,'') and
       location.hostname is this.hostname

      # need to reach a holder in the hierarchy that has a scrollbar
      holder = $("main").parent()
      if not (holder.css("overflowY") in ["scroll", "auto"])
        throw "main holder has no scroll"

      target = $(this.hash)
      holder.animate
        scrollTop: holder.scrollTop() + target.offset().top
      ,    
        duration: 600, 
        easing: "swing"

      # BUG: setting the location hash (directly or via pushState) "focuses"
      #      on the target ... including horizontally somehow, which is a pain
      #      in the ass ... Maybe I could set the lateral scroll just after
      #      the location change just to invert the move ? Or "defuse" the id.
      hash = this.hash
      target = $(hash)
      target.attr id: "__" + hash[1...] + "__"
      $("body").append empty = HTML.div
        id: hash[1...]
        css:
          position: "fixed"
          visibility: "hidden"
      location.hash = hash # Aah, nowhere to go now ! Shit, the bastard 
      # remembers the previous location ...
      empty.remove()
      target.attr id: hash[1...]
      
      #baseUrl = location.toString()[...location.hash.length]
      #window.history.pushState {}, "", baseUrl + this.hash

      return false

$ ->

  reset()

  body = $("body")
  html = $("html")
  body.css 
    overflow: "hidden"
  html.css overflow: "hidden"

  setupTOC()

  middleLeft = 
    css: {position: "fixed", top: "50%", left: "1em"}
  body.prepend HTML.MathJaxLoader(middleLeft).$

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


