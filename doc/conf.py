# Configuration file for the Sphinx documentation builder.
#
# https://www.sphinx-doc.org/en/master/usage/configuration.html
# https://alabaster.readthedocs.io/en/latest/customization.html

# -- Project information -----------------------------------------------------

project = 'haskell4nix'
copyright = '2020 Peter Simons'
author = 'Peter Simons'
release = '1'

# -- General configuration ---------------------------------------------------

extensions = ['recommonmark']
# templates_path = ['_templates']
exclude_patterns = ['_build', 'slides']

# This is the default in recent versions of Sphinx, but not in the version used
# by readthedocs.io.
master_doc = 'index'

# -- Options for HTML output -------------------------------------------------
#
html_theme = 'alabaster'
html_theme_options = {
    'canonical_url': 'https://hackage4nix.readthedocs.io/',
    'fixed_sidebar': True,
    'github_button': True,
    'github_repo': 'cabal2nix',
    'github_user': 'NixOS',
    'page_width': '100%',
    'show_related': False,
    'show_relbars': False,
    'sidebar_collapse': True,
    'sidebar_width': '30%',
    'travis_button': True,
}
#html_static_path = ['_static']
