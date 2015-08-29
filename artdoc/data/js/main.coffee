
# TODO
# ==============================================================================
#
#   - TOC switch suddenly very slow, investigate ... NB: slow only in firefox,
#     not in chrome. Linked to the switch of overlay css prop ? (doesn't work
#     as expected anyway).

# Generic Helpers
# ==============================================================================
type = (item) -> 
  Object::toString.call(item)[8...-1].toLowerCase();

# Method Observers
# ==============================================================================

# Mix into your object prototype to enable method observers.
connect = (source, target) ->
  source = source.name if source.name?

  this._targets or= {}
  targets = this._targets[source] or= []
  targets.push target
  
  method = this[source]
  if not method._connected?
    new_method = (args...) =>
      output = method.call(this, args...)
      for target in targets
        target(this, args...)
      return output 
    new_method._connected = on
    this[source] = new_method

  return undefined

# Icons
# ==============================================================================

extend = (base, diff) ->
  for k, v of diff
    bv = base[k]
    if k is "style" and bv? and type(v) is "object" # TODO: raise type error 
      # instead of doing smthin stupid when this is not an object
      extend(bv, v)
    else if k is "class" and bv?
      base[k] = [base[k], v].join(" ")
    else
      base[k] = v
  return base


# for the style, work with selector/style pairs instead to be able to
# reach inside nested components ? (and actually keeping most of the
# power of cascading style sheets ?). Actually, we could accept both
# styles (distinguish on the type, array or object). Argh, the
# diffing algo for the extension is likely to become a pure mess.
# And there is no graceful way to specify nesting either right ?
# I mean, could probably do something with a style in a style and
# somehow concatenate selectors, bit it would be brittle and complex.
class Component
  constructor: (options) ->
    if this.html? # error if undefined ?
      this.html.attr("id", options.id) if options.id?
      this.html.addClass(options.class) if options.class?
      this.html.css(options.style) if options.style?  

# TODO: define a configure static method instead of the constructor ?
#       Sure thing. Then, let go of the whole Component base class idea ?
#       Ask for an explicit html argument so that they will be no temptation
#       to call the stuff before html actually exists.

class Icon extends Component
  constructor: (type, options) ->
    this.html = html = $("<i></i>", class: "fa fa-" + type)
    super options

  spin: (status = on) ->
    if status
      this.html.addClass "fa-spin"
    else
      this.html.removeClass "fa-spin"

# tmp
#start = undefined

class MathJaxLoader extends Icon
  constructor: (options) ->
    options = extend MathJaxLoader.defaults(), options
    super "cog", options
    this.spin()

    # tmp ------------------------------------------------------------------------
#    MathJax.Hub.signal.Interest (msg) -> 
#      if not start?
#        start = (new Date()).valueOf()
#      if msg[0] is "New Math Pending"
#        console.log "MathJax: New Math Pending"
#      else if msg[0] is "New Math"
#        console.log "MathJax: New Math"
#      else
#        console.log "MathJax:", msg
#      if msg[0] is "End Process"
#        console.log "MathJax run:", ((new Date()).valueOf() - start) / 1000, "sec"
    # ----------------------------------------------------------------------------

    # this component, like, you know, removes itself when done ???
    MathJax.Hub.Register.StartupHook "End", => this.html.remove()

  this.defaults = () ->
    class:
      "mathjax-loader"
    style:
      fontSize: "1em" 
      position: "fixed"
      top: "50%"
      left: "1em"

# Table of Contents
# ==============================================================================

# Q: accept html argument to the constructor instead (the icons ?)
# Q: generic configurability ? (id, class, style ?) 

class Switch extends Component
  constructor: (@typeOn = "bars", @typeOff = "times", options) ->
    this.html = $("<i></i>", class: "fa")
    this.off()
    super options

  connect: connect

  on: ->
    this.html.removeClass("fa-" + this.typeOff).addClass("fa-" + this.typeOn)
    this.status = on

  off: ->
    this.html.removeClass("fa-" + this.typeOn).addClass("fa-" + this.typeOff)
    this.status = off

  toggle: (status = undefined) ->
    status ?= not this.status
    if status is on this.on() else this.off()

make_toc = (root) ->
  root ?= $("body")
  toc = $("<div></div>")
  header = root.children("header")
  if header.length == 0
    header = root
  heading = header.children("h1, h2, h3, h4, h5, h6")
  if heading.length
    item = $("<p>" + heading.first().html() + "</p>")
    item.appendTo(toc)
  sections = root.children("section")
  if sections.length
    list = $("<ul></ul>")
    list.appendTo(toc)
    sections.each (index, section) -> 
      (make_toc $(section)).appendTo(list)
  toc.addClass("toc")
  return toc
 
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

Hamburger = ->
  $("body").on "click", "#hamburger", (event) -> toc_switch()
  hamburger = $ "<i>", 
                id: "hamburger",
                class: "fa fa-bars",
                css:
                  fontSize: "1em" 
                  position: "fixed"
                  top: "1em"
                  left: "1em"
                  cursor: "pointer"
                  zIndex: 200

install_toc = () ->
  $("body").prepend Hamburger()
  $(".overlay").append make_toc()
  $(".toc a").on "click", (event) -> 
    toc_switch(off)
    true

# ------------------------------------------------------------------------------

switchOutline = (status) ->
  details = $(".remark, .theorem, .definition, .proof, .example, .generic, .footnotes")
  if status
    off
    # set the elements heights to their computed heights
    #details.each (i, elt) -> 
    #  $(elt).css {height: $(elt).height() / 2}
    # animate
    # details.animate {height: 0}, 2400
  #else 
    # err ... fuck. Memorize the height ? What if the context has changed ?

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

Overlay = -> $ "<div>",
    class: "overlay" 
    css:
      display: "none"
      position: "fixed"
      top: "0px"
      backgroundColor: "white"
      opacity: "1.0"
      zIndex: 100

$ ->
  body = $("body")
  body.append Overlay()
  body.prepend (new MathJaxLoader).html
  install_toc()

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


