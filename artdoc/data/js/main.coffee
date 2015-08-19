
# See jquery css: the std would be to use camel case instead.
attr = (object) ->
    (k.replace('_','-') + ":" + v for k, v of object).join("; ") 

MathJaxLoader = ->
  loader = $ "<i></i>", 
             id: "mathjax-loader",
             class: "fa fa-cog fa-spin",
             style: attr 
                      font_size: "1em" 
                      position: "fixed"
                      top: "50%"
                      left: "1em"
  # The "New Math Pending" and "New Math" messages 
  # (when newly typeset elements are added to the DOM ?)
  # is where we spend most of the time 
  # (see <http://docs.mathjax.org/en/latest/signals.html>)
  # and this is a phase where MathJax has already DONE its processing ?
  # This is just the browser dealing with slow DOM CSS & font handling ?
  # We have ~700 such elements.
  # Well, OK, that's "New Math Pending" that seems to take some time.
  # Note that Chrome renders a page in 6 secs when Firefox needs 14 sec.

  # Mmm, note that when I save the rendered file (ok, there are arguably
  # some Mathjax errors: "file load error", and some of the dynamic features
  # of MathJax do not work such as get text formula, or renderer selection,
  # BUT), the page seems OK, and MathJax needs only 1 sec or so to display it.

  start = undefined

  MathJax.Hub.signal.Interest (msg) -> 
    if not start?
      start = (new Date()).valueOf()
    console.log "MathJax:", msg[0]
    if msg[0] is "End Process"
      console.log "MathJax run:", ((new Date()).valueOf() - start) / 1000, "sec"
  MathJax.Hub.Register.StartupHook "End", -> loader.remove()
  return loader

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
 
toc_switch = (status=undefined) ->
  overlay = $(".overlay")
  status ?= overlay.css("display") is "none"
  if status == on
    overlay.css("display", "block")
    $("#toc-button").removeClass("fa-bars").addClass("fa-times")
  else
    overlay.css("display", "none")
    $("#toc-button").removeClass("fa-times").addClass("fa-bars")

make_toc_button = ->
  hamburger = $ "<i></i>", 
                id: "toc-button",
                class: "fa fa-bars",
                style: attr
                         font_size: "1em" 
                         position: "fixed"
                         top: "1em"
                         left: "1em"
                         cursor: "pointer"
                         z_index: 200
  $("body").on 
    click: (event) -> toc_switch(), 
    "#toc-button"
  return hamburger

install_toc = () ->
  $("body").prepend make_toc_button()
  $(".overlay").append make_toc() 
  $(".toc a").on "click", (event) -> 
    toc_switch(off)
    true

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

  
$ ->
  body = $ "body"
  body.append $("<div class='overlay' style='display:none;'></div>")
  body.prepend MathJaxLoader()
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


