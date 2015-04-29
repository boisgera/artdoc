# coding: utf-8

# Python 2.7 Standard Library
from __future__ import print_function
import atexit
import json
import pkg_resources
import shutil
import sys

# Third-Party Libraries
import lxml.html
import lxml.html.builder
from pathlib import Path
from plumbum import local
from plumbum.cmd import mkdir, rm, pandoc, scss

def info(*args, **kwargs):
    tab = kwargs.get("tab", 0)
    indent = tab * 2 * " "
    args_str = " ".join(unicode(arg) for arg in args)
    print(indent + args_str, file=sys.stderr)

subinfo = lambda *args: info(*args, tab=1)
subsubinfo = lambda *args: info(*args, tab=2)

WORKDIR = Path(".")
ARTDOC = WORKDIR / "artdoc"
if ARTDOC.exists():
    shutil.rmtree(str(ARTDOC))
DATA  = Path(pkg_resources.resource_filename(__name__, "data"))
atexit.register(pkg_resources.cleanup_resources)

HTML = DATA / "html"
CSS  = DATA / "css"
BIN  = DATA / "bin"

def get_metadata(filename):
    cmd = local[str(BIN / "metadata.hs")]
    return json.loads(cmd(filename))

def main():

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

    info("HTML template:")
    template_file = HTML / "index.html"
    subinfo(str(template_file))
    template = template_file.open().read().encode("utf-8")

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

    
    # TODO: iterate on the documents, generate the json file, filter it,
    #       generate the html file(s), all that in dist directory.
    #       Also need to: tweak the index template head & stuff to include
    #       the css & co that has been found, AND, we need to ship the resources ?
    #       Arf, that sucks, we'd better generate the HTML in workdir, so that
    #       we don't have to move the resources, just to somehow add the
    #       package data resource ('artdoc' package ?).


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

        # not working yet
        info("Flag/Box Proofs")
        cmd = local[str(BIN / "proof.hs")]
        subinfo(cmd, "< json > json")
        json_str = (cmd << json_str)()

        info("Convert Images to SVG Images")
        cmd = local[str(BIN / "svg.hs")]
        subinfo(cmd, "< json > json")
        json_str = (cmd << json_str)()



        ## TODO: section-tags and html5

        info("Generate HTML body")
        args = ["-f", "json", 
                "--mathjax", 
                "-t", "html"]
        cmd = pandoc[args]
        subinfo(cmd, "< json > body")
        body = (cmd << json_str)()

        # TODO: deal with a template that has no body instead ?
        html = (HTML / "index.html").open().read()
        html = lxml.html.document_fromstring(html)
        html_body = html.cssselect("body")[0]

        body = lxml.html.document_fromstring(body).cssselect("body")[0]

        html_body.attrib.update(body.attrib)
        html_body.extend(body[:])



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

     
        metadata = get_metadata(str(doc)) # "--email-obfuscation=none" ?

        #print(metadata)


        # ----------------------------------------------------------------------
        E = lxml.html.builder.E

        def to_HTML(text):
            if text is None:
                return []
            else:
                fragment = lxml.html.fragment_fromstring(text, create_parent=True)
                output = []
                if fragment.text is not None:
                    output.append(fragment.text)
                output.extend(fragment)
                return output 

        items = []

        date = to_HTML(metadata.get("date"))
        if date is not None:
            items.append(E.p({"class": "date"}, *date))

        title = to_HTML(metadata.get("title"))
        if title is not None:
            items.append(E.h1({"class": "title"}, *title))

        authors = metadata.get("author") or []

        for author in authors:
            if isinstance(author, basestring):
                name = to_HTML(author)
                email = None
                affiliation = None
            else:
                name = to_HTML(author.get("name"))
                email = to_HTML(author.get("email"))
                affiliation = to_HTML(author.get("affiliation"))

            if name is not None:
                if email is not None:
                    name = [E.a({"href": "mailto:" + email[0]}, *name)]
                name = E.h2({"class": "author"}, *name)
                items.append(name)
                if affiliation is not None:
                    affiliation = E.p({"class": "affiliation"}, *affiliation)
                    items.append(affiliation)
        
        header = E.header({"class": "main"}, *items)
 
        body = html.cssselect("body")[0]
        body.insert(0, header)

        # ----------------------------------------------------------------------

        
       

        html_str = lxml.html.tostring(html, encoding="utf-8", doctype="<!DOCTYPE html>")
        file = doc.with_suffix(".html").open("wb")
        file.write(html_str)

    sys.exit(0)

#    info("Generate the standalone HTML file")
#    output_file = doc_name + ".html"
#    with open(output_file, "w") as output:
#        output.write(template.replace("__BODY__", body));
#    subinfo("generated", output_file)
