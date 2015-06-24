
mathjax_loader = () ->
  # make something with an overlay instead ?
  # display the mathjax logo ?
  cog = $(
          "<i></i>", 
          {
           "id": "mathjax-working",
           "class": "fa fa-cog fa-spin",
           "style": "font-size:1em;" + 
                      "position:fixed;" +
                      "top:50%;" +
                      "left:1em;"
          }
         )
  cog.prependTo("body")
  MathJax.Hub.Register.StartupHook "End", 
    () -> $("#mathjax-working").remove()

outline = (root) ->
  
  root ?= $("body")
  output = $("<div></div>")
  
  header = root.children("header")
  if header.length == 0
    header = root
  heading = header.children("h1, h2, h3, h4, h5, h6")
  if heading.length
    item = $("<p>" + heading.first().html() + "</p>")
    console.log "item", item.prop("outerHTML")
    item.appendTo(output)
      
  sections = root.children("section")
  if sections.length
    console.log "sections", sections
    list = $("<ul></ul>")
    list.appendTo(output)
    sections.each (index, section) -> 
      (outline $(section)).appendTo(list) 

  output
  
make_toc = () ->
  sandwich = $(
          "<i></i>", 
          {
           "id": "mathjax",
           "class": "fa fa-bars",
           "style": "font-size:1em;" + 
                      "position:fixed;" +
                      "top:1em;" +
                      "left:1em;" +
                      "cursor:pointer;"
          }
         )
  sandwich.prependTo("body")
  sandwich.on("click", () -> $("toc").addClass("active"))
  
  console.log outline().prop("outerHTML")
  
  outline().appendTo("overlay")

$ () ->
  $("<div class='overlay'></div>").appendTo("body")
  mathjax_loader()
  make_toc()