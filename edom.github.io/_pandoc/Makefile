# This Makefile requires GNU make.

# Begin user-adjustable variables.
# The user may adjust these variables from the make command line or environment variables.

bib_file ?= $(CURDIR)/bib.bib

# Either "dev" or "prod".
env ?= dev

# The user overrides this if he/she wants to build some files only.
org_files ?= $(wildcard *.org)

# md_files is for sitemap, not for building with Pandoc.
# Markdown files are built by Jekyll, not Pandoc.
# These Markdown files should be converted to Org Mode with Pandoc,
# and then md_files should be deleted.
md_files ?= $(wildcard *.md)

html_dir ?= _pandoc_out/$(env)

site_url_prefix ?= https://edom.github.io

# End user-adjustable variables.



makefile_path = $(abspath $(lastword $(MAKEFILE_LIST)))

# $(prog_prefix) is the directory containing this Makefile.
# This has trailing slash.
prog_prefix ?= $(dir $(makefile_path))

template_file ?= $(prog_prefix)template-full.html
filter_file ?= $(prog_prefix)filter.lua
csl_file ?= $(prog_prefix)acm-sig-proceedings.csl
sitemap_file = $(html_dir)/sitemap.xml
html_files = $(addprefix $(html_dir)/,$(patsubst %.org,%.html,$(org_files)))

pandoc_flags_prod = \
	--metadata=production:true \
	--metadata=google_analytics_id:UA-12628443-6 \
	--metadata=site_url_prefix:$(site_url_prefix) \

.DELETE_ON_ERROR:

# Filter order: pandoc-citeproc must precede filter.lua.
#
# The citation style acm-sig-proceedings.csl was taken from:
# - https://www.zotero.org/styles/acm-sig-proceedings
# - https://github.com/citation-style-language/styles/blob/master/acm-sig-proceedings.csl

# 2018-12-27:
# We removed the dependency on $(bib_file), to prevent unnecessary rebuilding of the html files.
# This only works if we only add new entries to $(bib_file) and never update existing entries.
# If we (supposedly rarely) update an existing entry in $(bib_file), we will need "make -B".
$(html_dir)/%.html: \
		%.org \
		$(csl_file) \
		$(filter_file) \
		$(template_file) \

	@mkdir -p $(html_dir)
	pandoc \
		$(pandoc_flags_$(env)) \
        --metadata=link-citations:true \
		--metadata=page_file_name:"$(notdir $@)" \
		--mathjax \
        --bibliography="$(bib_file)" \
        --csl="$(csl_file)" \
        --filter=pandoc-citeproc \
        --lua-filter="$(filter_file)" \
        --template="$(template_file)" \
		--output $@ \
		$<
	! grep --fixed-strings --regexp=-PANDOC-TEMPLATE-ERROR- $@
	@echo

$(sitemap_file): \
		$(org_files) \
		$(md_files) \

	site_url_prefix=$(site_url_prefix) _sh/sitemap $^ > $@

.PHONY:	\
	all \
	sitemap \

all: \
	$(sitemap_file) \
	$(html_files) \

sitemap: $(sitemap_file)

# Jekyll _site directory.
jekyll_dir_dev = _site
jekyll_dir_prod = _site_publish

.PHONY: install
install: all
	rsync -a $(html_dir)/ $(jekyll_dir_$(env))
