# ccwl --- Concise Common Workflow Language
# Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of ccwl.
#
# ccwl is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ccwl is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ccwl.  If not, see <https://www.gnu.org/licenses/>.

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
%.go: %.scm
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

bin_SCRIPTS = scripts/ccwl

SOURCES =			\
  ccwl/ccwl.scm 		\
  ccwl/yaml.scm		        \
  ccwl/utils.scm

TEST_EXTENSIONS = .scm

SCM_TESTS =			\
  tests/ccwl.scm                \
  tests/yaml.scm

TESTS = $(SCM_TESTS)

SCM_LOG_DRIVER =		\
  $(top_builddir)/pre-inst-env	\
  $(GUILE) --no-auto-compile -s \
  $(top_builddir)/build-aux/test-driver.scm

EXTRA_DIST +=			\
  $(TESTS)                      \
  COPYING			\
  README.org

# Build documentation

SKRIBILO_BUILD_DIR = $(srcdir)/doc/skribilo

.depends: build-aux/find-dependencies.scm doc/ccwl.skb
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE) --no-auto-compile $< > $@

# We prefix with - so that automake does not fail when .depends is
# missing.
-include .depends

$(SKRIBILO_BUILD_DIR)/%.cwl: doc/%.scm
	$(MKDIR_P) $(SKRIBILO_BUILD_DIR)
	$(AM_V_GEN)$(top_builddir)/pre-inst-env ccwl compile $< > $@

# Print out graph for graphviz's dot, but remove file path prefix from
# workflow step identifiers.
%.dot: %.cwl
	$(CWLTOOL) --print-dot $< | $(SED) 's/file:[^#]*#//g' > $@

%.svg: %.dot
	$(DOT) -Tsvg -o$@ $<

# The info and html targets depend on the info-local and html-local
# targets respectively. So, we use them to extend the info and html
# targets.
info-local: $(SKRIBILO_BUILD_DIR)/ccwl.info
html-local: $(SKRIBILO_BUILD_DIR)/ccwl.html

$(SKRIBILO_BUILD_DIR)/ccwl.info: doc/ccwl.skb ccwl/skribilo.scm $(DOC_IMAGES) $(DOC_OTHER_DEPENDENCIES)
	$(MKDIR_P) $(SKRIBILO_BUILD_DIR)
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(SKRIBILO) -t info $< -o $@

$(SKRIBILO_BUILD_DIR)/ccwl.html: doc/ccwl.skb ccwl/skribilo.scm $(DOC_IMAGES) $(DOC_OTHER_DEPENDENCIES)
	rm -rf $@
	$(MKDIR_P) $@
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(SKRIBILO) -t html $< -o $@/index.html
	cp -vr $(DOC_IMAGES) $@

install-data-local: install-info-local

install-info-local: $(SKRIBILO_BUILD_DIR)/ccwl.info
	$(MKDIR_P) $(DESTDIR)$(infodir)
	$(INSTALL_DATA) $(SKRIBILO_BUILD_DIR)/ccwl.info $(DESTDIR)$(infodir)

install-html-local: $(SKRIBILO_BUILD_DIR)/ccwl.html
	$(MKDIR_P) $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $(SKRIBILO_BUILD_DIR)/ccwl.html $(DESTDIR)$(htmldir)

uninstall-local:
	rm -f $(DESTDIR)$(infodir)/ccwl.info $(DESTDIR)$(htmldir)/ccwl.html

# Build website

website: website/index.html website/manual/dev/en

website/index.html: README.org
	$(MKDIR_P) $(dir $@)
	$(EMACS) -Q --script build-aux/build-home-page.el

website/manual/dev/en: $(SKRIBILO_BUILD_DIR)/ccwl.html
	rm -rf $@
	$(MKDIR_P) $(dir $@)
	cp -vr $< $@

# The clean target depends on clean-local. We use clean-local to clean
# up the website and the skribilo build directory.
clean-local:
	rm -rf .depends website/index.html website/manual $(SKRIBILO_BUILD_DIR)
