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

# Prefix variables for silencing various commands. See "(automake)
# Automake Silent Rules" for details.
CCWL_GEN = $(CCWL_GEN_$(V))
CCWL_GEN_ = $(CCWL_GEN_$(AM_DEFAULT_VERBOSITY))
CCWL_GEN_0 = @echo "  CCWL" $@;
CCWL_GEN_1 =

CWLTOOL_GEN = $(CWLTOOL_GEN_$(V))
CWLTOOL_GEN_ = $(CWLTOOL_GEN_$(AM_DEFAULT_VERBOSITY))
CWLTOOL_GEN_0 = @echo "  CWLTOOL" $@;
CWLTOOL_GEN_1 =

DOT_GEN = $(DOT_GEN_$(V))
DOT_GEN_ = $(DOT_GEN_$(AM_DEFAULT_VERBOSITY))
DOT_GEN_0 = @echo "  DOT" $@;
DOT_GEN_1 =

EMACS_GEN = $(EMACS_GEN_$(V))
EMACS_GEN_ = $(EMACS_GEN_$(AM_DEFAULT_VERBOSITY))
EMACS_GEN_0 = @echo "  EMACS" $@;
EMACS_GEN_1 =

GUILD_GEN = $(GUILD_GEN_$(V))
GUILD_GEN_ = $(GUILD_GEN_$(AM_DEFAULT_VERBOSITY))
GUILD_GEN_0 = @echo "  GUILD" $@;
GUILD_GEN_1 =

SKRIBILO_GEN = $(SKRIBILO_GEN_$(V))
SKRIBILO_GEN_ = $(SKRIBILO_GEN_$(AM_DEFAULT_VERBOSITY))
SKRIBILO_GEN_0 = @echo "  SKRIBILO" $@;
SKRIBILO_GEN_1 =

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
	$(GUILD_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

bin_SCRIPTS = scripts/ccwl

SOURCES = ccwl/ccwl.scm ccwl/yaml.scm ccwl/utils.scm
TEST_EXTENSIONS = .scm
SCM_TESTS = tests/ccwl.scm tests/yaml.scm
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

CLEANFILES += .depends

.depends: build-aux/find-dependencies.scm doc/ccwl.skb
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE) --no-auto-compile $< > $@

# We prefix with - so that automake does not fail when .depends is
# missing.
-include .depends
CLEANFILES += doc/*.cwl doc/%.dot doc/*.svg

%.cwl: %.scm
	$(CCWL_GEN)$(builddir)/pre-inst-env ccwl compile $< > $@

# Print out graph for graphviz's dot, but remove file path prefix from
# workflow step identifiers.
%.dot: %.cwl
	$(CWLTOOL_GEN)$(CWLTOOL) --print-dot $< | $(SED) 's/file:[^#]*#//g' > $@

%.svg: %.dot
	$(DOT_GEN)$(DOT) -Tsvg -o$@ $<

# The info and html targets depend on the info-local and html-local
# targets respectively. So, we use them to extend the info and html
# targets.
info-local: doc/ccwl.info
html-local: doc/ccwl.html

CLEANFILES += doc/ccwl.info doc/skribilo.go
CLEAN_DIRECTORIES = doc/ccwl.html

doc/ccwl.info: doc/ccwl.skb doc/skribilo.go $(DOC_IMAGES) $(DOC_OTHER_DEPENDENCIES)
	$(SKRIBILO_GEN)$(top_builddir)/pre-inst-env $(SKRIBILO) -t info $< -o $@

doc/ccwl.html: doc/ccwl.skb doc/skribilo.go $(DOC_IMAGES) $(DOC_OTHER_DEPENDENCIES)
	rm -rf $@
	$(MKDIR_P) $@
	$(SKRIBILO_GEN)$(top_builddir)/pre-inst-env $(SKRIBILO) -t html $< -o $@/index.html
	cp -vr $(DOC_IMAGES) $@

# The install target depends on the install-data target, which in turn
# depends on the install-data-local target. Use the install-data-local
# target to install info documentation by default on `make install'.
install-data-local: install-info-local

# The install-info and install-html targets depend on the
# install-info-local and install-html-local targets respectively. Use
# them to install the skribilo generated info and html documentation.
install-info-local: doc/ccwl.info
	$(MKDIR_P) $(DESTDIR)$(infodir)
	$(INSTALL_DATA) $< $(DESTDIR)$(infodir)

install-html-local: doc/ccwl.html
	$(MKDIR_P) $(DESTDIR)$(htmldir)
	$(INSTALL_DATA) $< $(DESTDIR)$(htmldir)

# The uninstall target depends on the uninstall-local target. Use it
# to uninstall the installed info and html documentation.
uninstall-local:
	rm -f $(DESTDIR)$(infodir)/ccwl.info $(DESTDIR)$(htmldir)/ccwl.html

# Build website

website: website/index.html website/manual/dev/en

CLEANFILES += website/index.html
CLEAN_DIRECTORIES += website/manual
website/index.html: README.org
	$(EMACS_GEN)$(EMACS) -Q --script build-aux/build-home-page.el

website/manual/dev/en: doc/ccwl.html
	rm -rf $@
	$(MKDIR_P) $(dir $@)
	cp -vr $< $@

# The clean target depends on clean-local. Use clean-local to clean up
# several directories. Directories cannot be specified in CLEANFILES.
clean-local:
	rm -rf $(CLEAN_DIRECTORIES)
