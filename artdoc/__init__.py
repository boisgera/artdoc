# coding: utf-8

# Future 
from __future__ import print_function

# Python 2.7 Standard Library
import argparse
import atexit
import json
import pkg_resources
import shutil
import sys
import urllib

# Third-Party Libraries
import lxml.html
from lxml.html.builder import E as HTML
from pathlib import Path
from plumbum import local
from plumbum.cmd import mkdir, rm, pandoc, scss


# TODO: need the plumbum ProcessExecutionError to fail HARD, 
#       and give me proper detailled error messages.

# HTML Helpers
# ------------------------------------------------------------------------------

def parse_html(text=None):
    "Return the fragment contents as a list of text and element nodes"
    if text is None:
        return []
    else:
        fragment = lxml.html.fragment_fromstring(text, create_parent=True)
        output = []
        if fragment.text is not None:
            output.append(fragment.text)
        output.extend(fragment)
        return output 


# Google Web Fonts
# ------------------------------------------------------------------------------

GOOGLE_API_KEY = "AIzaSyCXe6WAu7i4CYL9ee-RFNZirObwT4zJyqI"

def google_fonts(*font_names):
    url = "https://www.googleapis.com/webfonts/v1/webfonts"
    info = json.loads(urllib.urlopen(url + "?key=" + GOOGLE_API_KEY).read())
    families = []
    for font_name in font_names:
        for font_info in info["items"]:
            if font_info["family"] == font_name:
                family = font_name.replace(" ", "+") + ":"
                variants = font_info["variants"]
                for i, v in enumerate(variants):
                    if v == "regular":
                        variants[i] = "400"
                    elif v == "italic":
                        variants[i] = "400italic"
                family += ",".join(variants)
                families.append(family)
                break
    family = "|".join(families) + "&subset=latin,latin-ext" 
    # shit, "&" and "|" are escaped ! "&" in every attribute, "|" only in
    # hrefs AFAICT. This is a somehow documented "bug" (see <https://github.com/peterbe/premailer/issues/72> for example.
    url = "http://fonts.googleapis.com/css?family=" + family
    return [HTML.link(rel="stylesheet", href=url)]



# ------------------------------------------------------------------------------
# TODO: use logfile instead (somehow).
def info(*args, **kwargs):
    tab = kwargs.get("tab", 0)
    indent = tab * 2 * " "
    args_str = " ".join(unicode(arg) for arg in args)
    print(indent + args_str, file=sys.stderr)

subinfo = lambda *args: info(*args, tab=1)
subsubinfo = lambda *args: info(*args, tab=2)
# ------------------------------------------------------------------------------

WORKDIR = Path(".")
ARTDOC = WORKDIR / ".artdoc"
if ARTDOC.exists():
    shutil.rmtree(str(ARTDOC))
DATA  = Path(pkg_resources.resource_filename(__name__, "data"))
atexit.register(pkg_resources.cleanup_resources)

CSS  = DATA / "css"
BIN  = DATA / "bin"

def get_metadata(filename):
    cmd = local[str(BIN / "metadata.hs")]
    return json.loads(cmd(filename))

# TODO: same pattern everywhere, change this: don't require the parent,
#       return the result as a list, let the caller do the integration.
def jquery(url="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"):
    return [HTML.script(src=url)]

def artdoc():
    return [
      HTML.link(rel="stylesheet", href=".artdoc/css/style.css"),
      HTML.script(src=".artdoc/js/main.js")
    ]

def mathjax(url="http://cdn.mathjax.org/mathjax/latest/MathJax.js", 
            config="TeX-AMS_HTML", 
            extra={"HTML-CSS": {"scale": 90},
                   "TeX": {"equationNumbers": {"autoNumber": "AMS"}}}):
    if config:
        url = url + "?config=" + config
    if extra is not None:
        js = "MathJax.Hub.Config({0})".format(json.dumps(extra))
    else:
        js = ""
    return [HTML.script(dict(src=url), js)] 

def font_awesome(url="http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"): 
    return [HTML.link(rel="stylesheet", href=url)]

def main():
    # TODO: combine command-line and option file.
    # TODO: option to generate a default configuration file
    parser = argparse.ArgumentParser() # TODO: doc
    parser.add_argument("-s", "--standalone", action="store_true") # TODO: doc
    args = parser.parse_args()
    standalone = args.standalone

    conf = json.load((DATA / "artdoc.js").open())

    if Path("artdoc.js").exists():
        user_conf = json.load(Path("artdoc.js").open())
        conf.update(user_conf)

    info("Document:")
    doc_patterns = conf["doc"]
    if isinstance(doc_patterns, basestring):
        doc_patterns = [doc_patterns]
    docs = []
    for pattern in doc_patterns:
        matches = list(WORKDIR.glob(pattern))
        #subinfo("matching {!r}:".format(pattern))
        for match in matches:
            subinfo(str(match))
        docs.extend(matches)
    if not docs:
        sys.exit("error: no document found")

#    info("HTML template:")
#    template_file = HTML / "index.html"
#    subinfo(str(template_file))
#    template = template_file.open().read().encode("utf-8")

    info("Bibliography:")
    bib_patterns = conf["bib"]
    if isinstance(bib_patterns, basestring):
        bib_patterns = [bib_patterns]
    bibs = []
    for pattern in bib_patterns:
        matches = list(WORKDIR.glob(pattern))
        #subinfo("matching {!r}:".format(pattern))
        for match in matches:
          subinfo(str(match))
        bibs.extend(matches)
    if not bibs:
        print()

    info("CSS:")
    css_patterns = conf["css"]
    if isinstance(css_patterns, basestring):
        css_patterns = [css_patterns]
    csss = []
    csss.append(CSS / "style.css")
    #subinfo("artdoc stylesheet:")
    subinfo(str(csss[0]))
    for pattern in css_patterns:
        matches = list(WORKDIR.glob(pattern))
        #subinfo("matching {!r}:".format(pattern))
        for match in matches:
            if match.suffix == ".css":
                if match not in csss:
                    subinfo(str(match))
                    csss.append(match)
            elif match.suffix == ".scss":
                css_file = match.with_suffix(".css")
                cmd = scss[str(match), str(css_file)]
                cmd()
                subinfo(str(css_file) + " (from " + str(match) + ")")
                csss.append(css_file)
            else:
                sys.exit("error: unknown stylesheet extension {!r}".format(match.suffix))
    new_csss = []
    for css in csss:
        if css not in new_csss:
            new_csss.append(css)
    csss = new_csss

    # TODO: copy only what is required.
    shutil.copytree(str(DATA), str(ARTDOC))

    for doc in docs:
        pass

        info("PANDOC: generate JSON file")
        args = ["-t", "json", "--smart"]
        for bib in bibs:
            args.extend(["--bibliography", str(bib)])
        args.append(str(doc))
        cmd = pandoc[args]
        subinfo(cmd, "> json")
        json_str = cmd()

        info("Convert raw TeX to raw HTML")
        cmd = local[str(BIN / "rawHTML.hs")]
        subinfo(cmd, "< json > json")
        json_str = (cmd << json_str)()

        info("Flag/Box Proofs")
        cmd = local[str(BIN / "proof.hs")]
        subinfo(cmd, "< json > json")
        try:
            json_str = (cmd << json_str)()
        except Exception as error:
            print(repr(error))

        info("Convert Images to SVG Images")
        cmd = local[str(BIN / "svg.hs")]
        subinfo(cmd, "< json > json")
        json_str = (cmd << json_str)()

        info("Generate HTML body from markdown")
        args = ["-f", "json", 
                "--mathjax", 
                "-t", "html5", "--section-divs"]
        cmd = pandoc[args]
        subinfo(cmd, "< json > body")
        pandoc_body_str = (cmd << json_str)()
        pandoc_html = lxml.html.document_fromstring(pandoc_body_str)
        pandoc_body = pandoc_html.cssselect("body")[0]

        info("Generate standalone HTML doc")
        html = HTML.html(HTML.head, HTML.body)
        body = html.cssselect("body")[0]
        head = html.cssselect("head")[0]
        head.append(HTML.meta(charset="utf-8"))
        body.attrib.update(pandoc_body.attrib)
        body.extend(pandoc_body[:])

        # ----------------------------------------------------------------------
        info("Add JQuery")
        head.extend(jquery())

        # ----------------------------------------------------------------------
        info("Add Google Fonts support")
        head.extend(google_fonts("Alegreya", "Alegreya SC"))

        # ----------------------------------------------------------------------
        info("Add Mathjax support")
        head.extend(mathjax())

        # ----------------------------------------------------------------------
        info("Add Font Awesome support")
        head.extend(font_awesome())

        # ----------------------------------------------------------------------
        info("Add artdoc css & js files")
        head.extend(artdoc())

        # ----------------------------------------------------------------------
        info("Setting language to english (required for hyphens)")
        html.set("lang", "en") 

        # ----------------------------------------------------------------------
        info("Turning headers into self-links")
        sections = html.cssselect("section")
        for section in sections:
            id_ = section.get("id")
            heading = None
            if len(section):
                first = section[0]
                if first.tag in "h1 h2 h3 h4 h5 h6".split():
                    heading = first
            if id_ and heading is not None:
                contents = [heading.text or ""] + heading[:]
                heading.text, heading[:] = None, []
                href = {"href": "#" + id_}
                link = HTML.a(href, *contents)
                heading.insert(0, link)

        # ----------------------------------------------------------------------


        # TODO: deal with metadata & insert a document header with:
        #   - title, 
        #   - date (format: Month Day, Year), autoformat, autogen ? 
        #   - author(s) (with mail & affiliation when available ?).
        #     Assume custom metadata or parse the author field ?
        #     Representation of multiple authors ? MMm eLIFEsciences use
        #     popup for author info. Ex: http://elifesciences.org/content/4/e06356 !
        #     here, use hints from http://dtd.nlm.nih.gov/book/tag-library/:
        #
        #       - name (don't be more precise)
        #       - affiliation (concatenate)
        #       - address ???
        #       - email  --> Font Awesome Icon
        #       - url / uri ?
        #       - form of ID ? (like HAL ? or ZBlatt ?)



        # TODO: look at the rendering of
        #       http://kieranhealy.org/blog/archives/2014/01/23/plain-text/:
        #         - small grey date on top, bold title, bold author name,
        #           italics affiliation, repeat.


        metadata = get_metadata(str(doc))

        items = []

        date = parse_html(metadata.get("date"))
        if date is not None:
            items.append(HTML.p({"class": "date"}, *date))

        title = parse_html(metadata.get("title"))
        if title is not None:
            items.append(HTML.h1({"class": "title"}, *title))
            head.insert(0, HTML.title(*title))

        authors = metadata.get("author") or []

        for author in authors:
            if isinstance(author, basestring):
                name = parse_html(author)
                email = None
                affiliation = None
            else:
                name = parse_html(author.get("name"))
                email = parse_html(author.get("email"))
                affiliation = parse_html(author.get("affiliation"))

            if name is not None:
                if email is not None:
                    name = [HTML.a({"href": "mailto:" + email[0]}, *name)]
                name = HTML.h2({"class": "author"}, *name)
                items.append(name)
                if affiliation is not None:
                    affiliation = HTML.p({"class": "affiliation"}, *affiliation)
                    items.append(affiliation)
        
        header = HTML.header({"class": "main"}, *items)
        body.insert(0, header)

        # ----------------------------------------------------------------------
        info("Generate the standalone HTML file")
        html_str = lxml.html.tostring(html, encoding="utf-8", doctype="<!DOCTYPE html>")
        doc.with_suffix(".html").open("wb").write(html_str)

    sys.exit(0)

