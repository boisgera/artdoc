#!/usr/bin/env python

# Python 2.7 Standard Library
from __future__ import print_function
import sys

# Third-Party Libraries
from pathlib import Path
from plumbum import local
from plumbum.cmd import mkdir, rm, pandoc, scss

def info(*args, **kwargs):
    tab = kwargs.get("tab", 0)
    indent = tab * 2 * " "
    args_str = " ".join(unicode(arg) for arg in args)
    print(indent + args_str, file=sys.stderr)

subinfo = lambda *args: info(*args, tab=1)

ROOT = Path(".")
HTML = ROOT / "html"
CSS  = ROOT / "css"

info("Search for a document")
for path in ROOT.glob("*.txt"):
    markdown = path
    doc_name = markdown.stem
    subinfo("found", repr(str(path)))
    break 

info("Search for a html template")
template_file = HTML / "index.html"
if not template_file.exists():
    sys.exit("template file 'html/index.html' not found")
else:
    subinfo("found", repr(str(template_file)))
    template = template_file.open().read().encode("utf-8")

info("Search for a bibliography")
for path in ROOT.glob("*.bib"):
    bibliography = path
    info("found", repr(str(bibliography)), tab=1)
    break
else:
    bibliography = None
    subinfo("found nothing")

info("SASS: generate CSS files")
for scss_file in CSS.glob("*.scss"):
    css_file = CSS / (scss_file.stem + ".css")
    cmd = scss[scss_file, css_file]
    subinfo(cmd)
    cmd()

info("PANDOC: generate JSON file")
args = ["-t", "json"]
if bibliography:
    args.extend(["--bibliography", bibliography])
args.append(markdown)
cmd = pandoc[args]
subinfo(cmd, "> json")
json = cmd()

info("Filter JSON file")
cmd = local["./bin/filter"]
subinfo(cmd, "< json > json")
json = (cmd << json)()

info("Generate HTML body")
args = ["-f", "json", 
        "--mathjax", 
        "-t", "html"]
cmd = pandoc[args]
subinfo(cmd, "< json > body")
body = (cmd << json)().encode("utf-8")

info("Generate the standalone HTML file")
output_file = doc_name + ".html"
with open(output_file, "w") as output:
    output.write(template.replace("__BODY__", body));
subinfo("generated", output_file)

