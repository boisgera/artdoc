
# TODO
# ==============================================================================
#
#   - TOC switch suddenly very slow, investigate ... NB: slow only in firefox,
#     not in chrome. Linked to the switch of overlay css prop ? (doesn't work
#     as expected anyway).

# Generic Helpers
# ==============================================================================
type = (item) ->
  toString = Object::toString.call
  toString(item)[8...-1].toLowerCase()

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

class Icon
  constructor: (type, options) ->
    this.html = html = $("<i></i>", class: "fa fa-" + type)
    configure this.html, extend(Icon.defaults(), options)

  this.defaults = () ->
    css:
      fontSize: "1em"

  spin: (status = on) ->
    if status
      this.html.addClass "fa-spin"
    else
      this.html.removeClass "fa-spin"

# TODO: should be a generic loader and MathJax related behavior externalized
#       Spinning behavior should also *probably* be defined here, not at the
#       icon level.
class MathJaxLoader extends Icon
  constructor: (options) ->
    options = extend MathJaxLoader.defaults(), options
    super "cog", options
    this.spin()
    MathJax.Hub.Register.StartupHook "End", => this.html.remove()

  this.defaults = () ->
    class:
      "mathjax-loader"

  debug: ->
    MathJax.Hub.signal.Interest (msg) -> 
      console.log "MathJax:", msg[0]

# Overlay
# ------------------------------------------------------------------------------
class Overlay

# Switch Button
# ------------------------------------------------------------------------------
class Switch
  constructor: (typeOff = "bars", typeOn = "times", options) ->
    this.typeOn = typeOn
    this.typeOff = typeOff
    this.html = $("<i></i>", class: "fa")
    this.off()
    configure this.html, (extend Switch.defaults(), options)
    this.html.on "click": => this.toggle()

  this.defaults = () ->
    class:
      "switch"
    css:
      fontSize: "1em"
      cursor: "pointer"

  connect: connect

  on: =>
    this.html.removeClass("fa-" + this.typeOff).addClass("fa-" + this.typeOn)
    this.status = on

  off: =>
    this.html.removeClass("fa-" + this.typeOn).addClass("fa-" + this.typeOff)
    this.status = off

  toggle: (status = undefined) =>
    status ?= not this.status
    if status is on 
      this.on() 
    else 
      this.off()

# Table of Contents
# ------------------------------------------------------------------------------
TOC = class TableOfContents
  constructor: (options) ->
    this.html = $("<nav></nav>")
    this.html.append TOC.getOutline()
    
    configure this.html, options

    this.html.show()

  show: =>
    this.html.css display: "block"
    this.html.focus()

  hide: =>
    this.html.css display: "none"

  this.getOutline = (root = undefined, type = "ul") ->
    root ?= $("body")
    list = $("<#{type}></#{type}>")

    headers = root.children("header")
    if headers.length == 0
      headers = root
    headings = headers.children("h1, h2, h3, h4, h5, h6")
    if headings.length
      item = $("<p>" + headings.first().html() + "</p>")
      item.appendTo(list)

    sections = root.children("section")
    if sections.length
      subList = $("<#{type}></#{type}>")
      subList.appendTo(list)
      sections.each (index, section) -> 
        (TOC.getOutline $(section), type).appendTo(subList)

    return list

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
 
# This conflates two things: the change in the state of the button and looks
# which should be internal to the Button, and the consequence on the TOC,
# which should be externally managed. The button may export the "on", 
# "off" and "switch" event for example, as well as status attribute.
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

setupTOC = () ->
  topLeft = 
    css: {position: "fixed", top: "1em", left: "1em", zIndex: 200}
  hamburger = new Switch "bars", "times", topLeft
  $("body").prepend hamburger.html

  toc = new TableOfContents css: 
    position: "fixed",
    top: 0
    left: 0
    zIndex: 0
    backgroundColor: "yellow"
    paddingLeft: "2em"
  $("body").append toc.html

  hamburger.connect(on: toc.show, off: toc.hide).off()

  # debug
  window.hamburger = hamburger
  window.toc = toc


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

smoothLinks = ->  
  $("a[href*=#]:not([href=#])").click ->
    if location.pathname.replace(/^\//,'') is this.pathname.replace(/^\//,'') and
       location.hostname is this.hostname
      target = $(this.hash)
      console.log "top:", target.offset().top
      target.velocity "scroll",
        #top: target.offset().top, 
        duration: 600,
        easing: "easeOutCubic",
      return false

$ ->
  body = $("body")

  middleLeft = 
    css: {position: "fixed", top: "50%", left: "1em"}
  body.prepend (new MathJaxLoader middleLeft).html

  setupTOC()

  smoothLinks()
  
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


