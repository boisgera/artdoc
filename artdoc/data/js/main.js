
function mathjax() {
  var icon = $(
               "<i></i>", 
               {
                 "id": "mathjax-working",
                 "class": "fa fa-cog fa-spin",
                 "style": "font-size:1em;" + 
                          "position:fixed;" +
                          "top:1em;" +
                          "left:1em;"
               }
             ).prependTo("body");

   MathJax.Hub.Register.StartupHook(
     "End", 
     function () {
       $("#mathjax-working").remove();
     });
}

$(function () {
  mathjax();
});
