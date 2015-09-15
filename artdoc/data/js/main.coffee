
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
  $("html, body").css
    margin: "0px"
    padding: "0px"

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
    textAlign: "justify"
    textRendering: "optimizeLegibility"
    hyphens: "auto"
    MozHyphens: "auto"

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
      marginTop: "0px"
      marginBottom: "0px"

#      &::after
#        content: "."



  $("h1", "main header").css 
    fontSize: fontSize.huge
    fontWeight: "bold"

  $("h2", "main header").css
    fontSize: fontSize.xLarge
    fontWeight: "bold"

  

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
        console.log "error", tag, attributes, children
        throw "invalid child."
      new_children.push child
    return [attributes, new_children]


# HTML Builder
# ==============================================================================
HTML = (cls) -> # add Component classes as factories the HTML namespace.
  HTML[cls.name] = (args...) -> new cls(args...)

for tag in "a aside body div em h1 h2 h3 h4 h5 i main nav p span strong".split(" ")
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
      overflowY: "scroll"
      overflowX: "hidden"
      position: "absolute",
      transition: "transform 1.2s cubic-bezier(0.0,0,0.25,1)"
    divs = []

    for item, i in items
      console.log "item", item
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
HTML class TOC extends Component
  constructor: (options) ->
    root = options.root
    options.root = undefined

    super "nav", options

    this.$.append HTML.nav TOC.outline(root)
    this.show()

  show: =>
    this.$.css display: "block"
    this.$.focus()

  hide: =>
    this.$.css display: "none"

  # Remark: we rely ONLY on section tags for nesting, not headings levels.
  # Q: question the quite deeply nested ul/li design.
  this.outline = (root = undefined) ->
    root ?= $("body")    

    #console.log "root", root, root.children()

    list = $("<ul></ul>")
    root.children().each ->
      tag = this.nodeName.toLowerCase()
      #console.log "tag", tag
      if tag in "h1 h2 h3 h4 h5 h6".split(" ")
        text = $(this).html()
        #console.log tag, text
        list.append $("<li><p>#{text}</p></li>") if text
        #console.log "list", list.prop("outerHTML")
      else if tag is "header"
        #console.log "header"
        outline = TOC.outline $(this)
        #console.log "outline first:", outline.first().prop("outerHTML")
        list.append outline.children().first() # How can that fuck the whole execution ?
        list.append outline.children().clone()
        #outline.each -> list.append $(this)
      else if tag is "div"
        #console.log "div"
        outline = TOC.outline $(this)
        list.append outline.children() if outline.children().length > 0
      else if tag is "section"
        outline = TOC.outline $(this)
        list.append $("<li></li>").append(outline) if outline.children().length > 0
    #console.log "list", list
    return list

#    headers = root.children("header")
#    if headers.length == 0
#      headers = root
#    headings = headers.children("h1, h2, h3, h4, h5, h6")
#    if headings.length
#      item = $("<p>" + headings.first().html() + "</p>")
#      item.appendTo(list)

#    sections = root.children("section")
#    if sections.length
#      subList = List
#      subList.appendTo(list)
#      sections.each (index, section) -> 
#        (TOC.getOutline $(section), type).appendTo(subList)


#make_toc = (root) ->
#  root ?= $("body")
#  toc = $("<div></div>")
#  header = root.children("header")
#  if header.length == 0
#    header = root
#  heading = header.children("h1, h2, h3, h4, h5, h6")
#  if heading.length
#    item = $("<p>" + heading.first().html() + "</p>")
#    item.appendTo(toc)
#  sections = root.children("section")
#  if sections.length
#    list = $("<ul></ul>")
#    list.appendTo(toc)
#    sections.each (index, section) -> 
#      (make_toc $(section)).appendTo(list)
#  toc.addClass("toc")
#  return toc
 
# TODO: Overlay & Closed TOC vs Pinned model and shifted content. Can I have both ?
# TODO: how to handle TOC scrolling *in both modes* ? Is scrollin inconsistent with
#       the fixed position approach ? Think of some inline-block stuff instead ?
# TODO: clean Overlay & Close solution first (with scroll solved), think of pinned later ?
#       Search for classic two-column solutions that I could tweak (inline-box or
#       flexbox, whatever)

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

# TODO: opacity (grey) on top of the document, shifted to the right.
#       change of the cursor when we hower on top of this part with
#       a cross, clicking does collapse the TOC.

# TODO: solve the navigation in TOC pb (need to be able to scroll).

# TODO: management of selected section.

# TODO: the toc should belong to an overlay that may partially hide the content.

#_setupTOC = () ->
#  # specil treatment for the top-level header ?
#  content = $("<div></div>", class: "content").append $("body").children()
#  $("body").append content

#  topLeft = 
#    css: {position: "fixed", top: "1em", left: "1em", zIndex: 900}
#  hamburger = new Switch "bars", "times", topLeft
#  $("body").prepend hamburger.html

#  toc = new TOC css: 
#    position: "fixed",
#    top: 0
#    left: 0
#   #zIndex: 200
#    backgroundColor: "white"
#    #border: "1px solid black"
#    paddingLeft: "2em"
#  $("body").prepend toc.html

#  hamburger.connect(on: toc.show, off: toc.hide).off()

#  # debug
#  window.hamburger = hamburger
#  window.toc = toc

setupTOC = () ->

  # Transfer the body contents from body to main
  main = HTML.main 
    css:
      margin: "auto"
      maxWidth: "32em"
      width: "auto"
      height: "auto" 
    $("body").children()

  # Add the hambuger button.
  topLeft = {position: "fixed", top: "1em", left: "1em", zIndex: 999}
  hamburger = HTML.Switch typeOff: "bars", typeOn: "times", css: topLeft
  $("body").prepend hamburger.$

  # Create the table of contents
  toc = HTML.TOC
    root: 
      main
    css: 
      position: "fixed"
      top: 0
      left: 0
      backgroundColor: "white"
      paddingLeft: "2em"
  
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

  hamburger.connect
    on: -> panel.setIndex 0
    off: -> panel.setIndex 1
  hamburger.off()




# ------------------------------------------------------------------------------

testAnim = () ->
   duration = 1200
   # This removal is not that bad, but it does interfere with the margin collapse
   # of the elements just before and after (if any), making them look too far apart.
   # also, that's funny, just when I click, some elements (not all ?) are translated
   # to the bottom. Why is that ? Ah, that may be the overflow hidden trigger that
   # does stuff with the margins (does not the p margins to go beyond the div extent)
   # Yes: now that we don't allow "content" to go beyond the div, the margin used
   # for the ps just finds it way inside the div, effectively creating an extra margin.
   # (this margin is not collapsable anymore).
   # Could we compute the new height after overflow: hidden and compensate for it ?
   # Or directly find a way to compensate beforehand, but "it depends" on the context.
   # Could we "transfer" the margin if the first element of the div to the div itself ?
   # (and compensate inside with a negative padding ?)
   # and same thing for the last one ? Mmmm maybe ...
   elts = $(".remark, .theorem, .definition, .proof, .example, .generic, .footnotes")
   elts.each (i, elt) ->
     elt = $(elt)
     elt.on "click", (event) ->
       firstChild = elt.children().first()
       marginTop = firstChild.css("margin-top")
       lastChild = elt.children().last()
       marginBottom = lastChild.css("margin-bottom")
       firstChild.css
         marginTop: "0px"
       lastChild.css
         marginBottom: "0px"
       elt.css 
         overflow: "hidden", 
         height: elt.height(), 
         marginTop: marginTop
       console.log "1st step"
       elt.animate 
         height: 0,
       ,
         duration: duration, 
         complete: ->
           console.log "2nd step"
           elt.animate 
             marginTop: 0, 
             marginBottom: 0,
           , 
             duration: duration,
             complete: -> console.log "done"
 
       # does not work, read https://learn.jquery.com/effects/queue-and-dequeue-explained/

       # TODO: make the animation scroll at a constant speed (the longer the
       # div, the longer the animation)

       # TODO: when done, animate remaining margins into oblivion ?
       # Well, there is the "constant speed" issue and then, there is STILL
       # the collapsing margin issue ... we may need to compensate for the 
       # presence of the block by setting negative margins.

       # TODO: have a look at jquery plugins wrt animations

highlight = ->
  selection = window.getSelection()
  console.log selection 

  
# this is ugly, try another easing or JQuery animate. Do manual shit with
# requestAnimationFrame instead ?
# TODO: measure the distance & take more time if we are far (~constant speed).

#<http://www.adriantomic.se/development/jquery-localscroll-tutorial/>
smoothLinks = ->  
  $("a[href*=#]:not([href=#])").click (event) ->
    if location.pathname.replace(/^\//,'') is this.pathname.replace(/^\//,'') and
       location.hostname is this.hostname

      # need to reach the holder with a scrollbar (here hardcoded)
      holder = $("main").parent().parent() 
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

  smoothLinks()
  
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


