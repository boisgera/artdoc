
General Idea: handle LaTeX-heavy article-like markdown document to HTML conversion.

  - deal with the analysis & use of metadata (ex: title, date, author, etc.)

  - deployment scheme for the package ?

  - manage citations

  - deal with `equation` environment (from RawLatex to RawHTML ?). 
    Because now MathJax supports this !
    Analyze the interaction with references to the equation ?
    OK, found it, BUT the color of the references to the eq are still
    an issue (can get rid of the blue). OOOOOOOOK, works by default
    if I use the HTML-CSS render, but the SVG one has a pb. Fair enough.
 
    Update: I have replaced latex raw blocks with html raw blocks and it works,
    but it uses pandoc library for the moment :(. I'd rather use only Python
    (pandoc executable is ok, install of the pandoc libs in a usable way is
    so-so), but there are still issues in the Python pandoc module
    (Citation struct to be precise), so the temporary solution will have to
    do.

  - manage images: width, centering, etc. Use width or max-width ???
    width would be simpler to deal with (no centering issue).

  - manage figure aka images + caption.

  - deal with pdf that where generated with pdf backend (ouch).
    Assume the existence of svg and replace the stuff ? Yes, this is the
    most promising lead.

  - deal with title stuff (title, author, date, etc.)

  - deal with toc,

  - styling of quotes,

  - deal with "--" that have not been replaced.

  - manage "back to the top" with elevator.js :)

  - implement a "grey zone" for the block that is not in focus (to deal
    with the document as slides). May be complex, "block" is not a simple
    notion. But the document just looks fantastic when zoomed in, it could
    TOTALLY be used as a presentation.

  - format bibliography. Maybe link the citations to the bibliography ?

  - table of contents / outline computation and (floating) menu stuff.

