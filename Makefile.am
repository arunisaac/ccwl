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
info_TEXINFOS = doc/ccwl.texi

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

# Build website

AM_MAKEINFOHTMLFLAGS = --css-ref=/style.css

website: website/index.html website/style.css website/manual/dev/en

website/index.html: README.org
	mkdir -p $(dir $@)
	emacs -Q --script build-aux/build-home-page.el

website/style.css: style.css
	mkdir -p $(dir $@)
	cp -v $< $@

website/manual/dev/en: doc/ccwl.html
	rm -rf $@
	mkdir -p $(dir $@)
	cp -vr doc/ccwl.html $@

# The clean target depends on clean-local. We use clean-local to clean
# up the website built by the website target.
clean-local:
	rm -rf website
