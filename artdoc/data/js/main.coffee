
mathjax = () ->
  cog = $(
          "<i></i>", 
          {
           "id": "mathjax-working",
           "class": "fa fa-cog fa-spin",
           "style": "font-size:1em;" + 
                      "position:fixed;" +
                      "top:1em;" +
                      "left:1em;"
          }
         )
  cog.prependTo("body")
  MathJax.Hub.Register.StartupHook "End", 
    () -> $("#mathjax-working").remove()

outline = (root) ->
  
  root ?= $("body")
  console.log "root", root

  output = $("<div></div>")
  
  header = root.children("header")
  if header.length == 0
    header = root
  heading = header.children("h1, h2, h3, h4, h5, h6")
  if heading.length
    item = $("<p>" + heading.first().html() + "</p>")
    console.log "item", item.prop("outerHTML")
    item.appendTo(output)
    
  console.log "output:", output.prop("outerHTML")
  
  sections = root.children("section")
  if sections.length
    console.log "sections", sections
    list = $("<ul></ul>")
    list.appendTo(output)
    sections.each (index, section) -> 
      (outline $(section)).appendTo(list) 
    console.log "list", list
  
  console.log "exiting", root
  console.log "output:", output.prop("outerHTML")
  output
  
$ () -> 
  mathjax()
  outline().appendTo("body")