#!/usr/bin/env python
# coding: utf-8

import setuptools
import setuptools_git

# Setup
# ------------------------------------------------------------------------------

metadata = dict(
  name         = "artdoc",
  version      = "0.0.0",
  author       = u"Sébastien Boisgérault",
  author_email = "Sebastien.Boisgerault@mines-paristech.fr",
  license      = "MIT",
)

code = dict(
  packages = setuptools.find_packages(),
)

data = dict(
  include_package_data = True,
)

scripts = dict(
  entry_points = {"console_scripts": ["artdoc=artdoc:main"]}
)

info = dict(
  metadata = metadata,
  code     = code,
  data     = data,
  scripts  = scripts,
)

if __name__ == "__main__":
    kwargs = {k:v for dct in info.values() for (k,v) in dct.items()}
    setuptools.setup(**kwargs)

