# ccwl --- Concise Common Workflow Language
# Copyright © 2022, 2024 Arun Isaac <arunisaac@systemreboot.net>
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

include Makefile.include

DOT = dot
EMACS = emacs
GIT = git
GPG = gpg
GUILD = guild
GUILE = guile
LZIP = lzip
SKRIBILO = skribilo

FIND_DEPENDENCIES = build-aux/find-dependencies.scm
GENERATE_CWL_OUTPUT = build-aux/generate-cwl-output.sh
TEST_DRIVER = build-aux/test-driver.scm

top_level_module_dir = $(project)
sources = $(wildcard $(top_level_module_dir)/*.scm)
objects = $(sources:.scm=.go)
scripts = $(wildcard scripts/*)
tests = $(wildcard tests/*.scm)
test_data = $(wildcard tests/*.cwl)
doc_sources = doc/ccwl.skb
doc_info = $(doc_sources:.skb=.info)
doc_html = $(doc_sources:.skb=.html)
doc_data = doc/hello.c.gz doc/hello.tar doc/hello.txt \
           doc/spell-check-text.txt doc/dictionary
fonts = $(addprefix $(GUIX_ENVIRONMENT)/share/fonts/web/, charter_regular.woff2 FiraCode-Regular.woff2 FiraCode-SemiBold.woff2)
distribute_files = $(sources) $(scripts) $(tests) $(test_data) \
                   $(doc_sources) doc/skribilo.scm $(doc_data) $(DOC_SCM) $(DOC_OTHER) \
                   pre-inst-env guix.scm Makefile configure configure.scm \
		   $(FIND_DEPENDENCIES) $(GENERATE_CWL_OUTPUT) $(TEST_DRIVER) \
		   COPYING NEWS.org README.org

scmdir = $(datarootdir)/guile/site/$(guile_effective_version)/$(top_level_module_dir)
godir = $(libdir)/guile/$(guile_effective_version)/site-ccache/$(top_level_module_dir)

.PHONY: all check clean dist distcheck info install

# Build

all: $(objects)

%.go: %.scm
	GUILE_AUTO_COMPILE=0 $(GUILD) compile -L . -o $@ $<

# Run tests

check: $(tests) $(TEST_DRIVER)
	$(GUILE) --no-auto-compile -L . $(TEST_DRIVER) $(tests)

# Build documentation

include .depends

.depends: $(doc_sources) $(FIND_DEPENDENCIES)
	$(GUILE) --no-auto-compile $(FIND_DEPENDENCIES) > $@

info: $(doc_info)
html: $(doc_html)

doc:
	mkdir -p $(dir $@)

$(DOC_IMAGES:.png=.dot) $(DOC_OUT): | doc

%.cwl: %.scm
	./pre-inst-env ccwl compile $< > $@

doc/capture-output-file.out: doc/capture-output-file.cwl doc/hello.tar $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --archive $(word 2, $^)

doc/capture-output-file-with-parameter-reference.out: doc/capture-output-file-with-parameter-reference.cwl doc/hello.tar $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --archive $(word 2, $^) --extractfile hello.txt

doc/capture-stdout.out: doc/capture-stdout.cwl $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --message '"Hello World!"'

doc/checksum.out: doc/checksum.cwl doc/hello.txt $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --file $(word 2, $^)

doc/decompress-compile-run.out: doc/decompress-compile-run.cwl doc/hello.c.gz $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --compressed-source $(word 2, $^)

doc/hello-world.out: doc/hello-world.cwl $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --message '"Hello World!"'

doc/pass-stdin.out: doc/pass-stdin.cwl doc/hello.txt $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --file $(word 2, $^)

doc/spell-check.out: doc/spell-check.cwl doc/spell-check-text.txt doc/dictionary $(GENERATE_CWL_OUTPUT)
	$(GENERATE_CWL_OUTPUT) $< --text-file $(word 2, $^) --dictionary $(word 3, $^)

doc/hello.tar.out: doc/hello.tar
	echo "$$ tar --list --file $(notdir $<)" > $@
	tar --list --file $< >> $@

%.dot: %.scm
	./pre-inst-env ccwl compile --to=dot $< > $@

%.png: %.dot
	$(DOT) -Tpng -o$@ $<

$(doc_info): $(doc_sources) doc/skribilo.go $(DOC_IMAGES) $(DOC_SCM) $(DOC_OUT) $(DOC_OTHER)
	./pre-inst-env $(SKRIBILO) --target=info $< --output=$@

$(doc_html): $(doc_sources) doc/skribilo.go $(DOC_IMAGES) $(DOC_SCM) $(DOC_OUT) $(DOC_OTHER)
	rm -rf $@
	mkdir -p $@
	./pre-inst-env $(SKRIBILO) --target=html $< --output=$@/index.html
	cp -v $(DOC_IMAGES) $@

# Install

install: $(sources) $(objects) $(scripts) $(doc_info)
	install -D $(sources) --target-directory $(scmdir)
	install -D $(objects) --target-directory $(godir)
	install -D $(scripts) --target-directory $(bindir)
	install -D $(doc_info) --target-directory $(infodir)

# Build distribution tarball

dist_archive = $(project)-$(version).tar.lz

dist: $(dist_archive) $(dist_archive).asc

$(dist_archive): .git/refs/heads/main
	$(GIT) archive --prefix $(basename $(basename $@))/ --format=tar main $(distribute_files) \
		| $(LZIP) --force --output $@

%.asc: %
	$(GPG) --detach-sign --armor $<

distcheck: $(dist_archive)
	$(GUILE) build-aux/distcheck.scm $(version) $<

# Build website

website: website/index.html website/manual/dev/en website/fonts

website/index.html: README.org build-aux/build-home-page.el
	$(EMACS) -Q --batch --load build-aux/build-home-page.el --funcall build-website

website/manual/dev/en: $(doc_html)
	rm -rf $@
	mkdir -p $(dir $@)
	cp -vr $^ $@

website/fonts: $(fonts)
	rm -rf $@
	mkdir -p $@
	cp -v $^ $@/

# Clean

clean:
	rm -f .$(objects) $(dist_archive) $(dist_archive).asc \
	      .depends Makefile.include website/index.html \
              $(DOC_SCM:.scm=.cwl) $(DOC_IMAGES) $(DOC_IMAGES:.png=.dot) $(DOC_OUT) \
	      $(doc_info) doc/skribilo.go
	rm -rf $(doc_html) website/manual website/fonts
