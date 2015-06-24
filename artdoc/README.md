
General Idea: handle LaTeX-heavy article-like markdown document to HTML conversion.


  - TODO: notification region on the right that comes & goes (animation).
    Use a card. Ex: Mathjax with cog during the processing, then check.

  - TODO: study an "outline mode" that would work as a toc too: details
    (such as paragraphs) disappear, titles are shrinked, etc, the 
    transformation is animated. Nagivate, then go back to "document" mode
    (or slide mode BTW)

  - TODO: slide mode

  - TODO: "active(s) sections" & what can be done with them.
    For example, in toc mode, highlight / position the current section
    or subsection at the top.

  - need to deal with two image size policy: either "fit width", or use the same
    scale than for the LaTeX document (ex: when TeX fonts are embedded).
    Needs to be done for pdf, svg, png, etc.

  - "lightbox"-style pop out for figures, or more generally, "blocks" ?.

  - code blocks that insert "..." when too wide & expand or pop out when hovered 
    or clicked.

  - biblio: make good use of urls in the bib (does it work now ?), make
    sure that reference to the bibitems actually redirect to the bib items.

  - css hyphens.

  - deal with the analysis & use of metadata (ex: title, date, author, etc.)

  - find a way to deal with $\hfill$ (before the tombstone).

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

  - dropcaps ? (https://github.com/adobe-webplatform/dropcap.js)

  - smart underline (to be tweaked for Alegreya).

  - floating figures (aside ?) ? Code used to generate the figures ?

  - (Fuzzy) text search and scroll / highlight ?

  - work on the automatic sectioning of the document 

  - special detection / handling of some blocks (such as Proofs).

  - transform highlighted selection into large, blockquote-like blocks.

  - the generation of PDF inside HTML is awful. Can we do better ?

