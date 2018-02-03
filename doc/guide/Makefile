GIT := git
MLTON_VERSION := $(shell TZ=UTC $(GIT) log -n1 --date=format-local:"%Y%m%d.%H%M%S" --pretty=format:"%cd-g%h$$([ "$$($(GIT) status --porcelain 2> /dev/null)" ] && echo '-dirty')" 2> /dev/null || echo '????????')

export LC_ALL = C

NULL :=

ASCIIDOC := asciidoc
ifneq ($(shell which $(ASCIIDOC) 2> /dev/null),)
ASCIIDOC_CONF_DIR := $(shell $(ASCIIDOC) -v /dev/null 2>&1 | head -n 1 | sed 's|.*reading: \(.*\)/asciidoc.conf|\1|')
else
ASCIIDOC_CONF_DIR :=
endif
ASCIIDOC_MLTON_FLAGS := $(shell cat conf/asciidoc-mlton.flags | sed 's|^\#.*||' | sed "s|MLTON_VERSION|$(MLTON_VERSION)|")
ASCIIDOC_MLTON_DEPS := bin/InclGitFile.py conf/asciidoc-mlton.flags $(shell cat conf/asciidoc-mlton.flags | sed -n 's|^.*\(conf/.*\)$$|\1|p')
ASCIIDOC_HTML5_FLAGS := $(ASCIIDOC_MLTON_FLAGS) $(shell cat conf/asciidoc-html5.flags | sed 's|^\#.*||')
ASCIIDOC_HTML5_DEPS := $(ASCIIDOC_MLTON_DEPS) conf/asciidoc-html5.flags $(shell cat conf/asciidoc-html5.flags | sed -n 's|^.*\(conf/.*\)$$|\1|p')
ASCIIDOC_DOCBOOK45_FLAGS := $(ASCIIDOC_MLTON_FLAGS) $(shell cat conf/asciidoc-docbook45.flags | sed 's|^\#.*||')
ASCIIDOC_DOCBOOK45_DEPS := $(ASCIIDOC_MLTON_DEPS) conf/asciidoc-docbook45.flags $(shell cat conf/asciidoc-docbook45.flags | sed -n 's|^.*\(conf/.*\)$$|\1|p')

DBLATEX := dblatex

GEN_PAGES := Index mlton-guide
GEN_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png Logo.attachments/mlton-$(size).pdf)

SRC_PAGES := $(filter-out $(GEN_PAGES),$(patsubst ./%.adoc,%,$(shell cd src ; find . -type f -name '*.adoc')))
SRC_ATTACHMENTS := $(filter-out $(GEN_ATTACHMENTS),$(patsubst ./%,%,$(foreach dir,$(shell cd src ; find . -type d -name '*.attachments'),$(shell cd src ; find $(dir) -type f ! -name '.gitignore' ! -name '.gitattributes'))))

HTML5_XTRA := asciidoc.css asciidoc.js pygments.css mlton.css mlton-gcse.js index.html
HTML5_XTRA_PAGES := Index

MLTON_ORG_XTRA := $(HTML5_XTRA)
MLTON_ORG_XTRA_PAGES := $(HTML5_XTRA_PAGES)
MLTON_ORG_XTRA_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png)
MLTON_ORG := $(addprefix www.mlton.org/, $(SRC_PAGES) $(SRC_ATTACHMENTS) $(MLTON_ORG_XTRA) $(MLTON_ORG_XTRA_PAGES) $(MLTON_ORG_XTRA_ATTACHMENTS))
MLTON_ORG_TORM := $(filter-out $(MLTON_ORG),$(shell if [ -d www.mlton.org ]; then find www.mlton.org -type f; fi))

LOCALHOST_XTRA := $(filter-out mlton-gcse.js,$(HTML5_XTRA))
LOCALHOST_XTRA_PAGES := $(HTML5_XTRA_PAGES)
LOCALHOST_XTRA_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png)
LOCALHOST := $(addprefix localhost/, $(SRC_PAGES) $(SRC_ATTACHMENTS) $(LOCALHOST_XTRA) $(LOCALHOST_XTRA_PAGES) $(LOCALHOST_XTRA_ATTACHMENTS))
LOCALHOST_TORM := $(filter-out $(LOCALHOST),$(shell if [ -d localhost ]; then find localhost -type f; fi))


ALL :=
ifneq ($(shell which $(ASCIIDOC) 2> /dev/null),)
ifneq ($(shell which pygmentize 2> /dev/null),)
ALL += $(LOCALHOST) localhost-rm
endif
ifneq ($(shell which $(DBLATEX) 2> /dev/null),)
ALL += mlton-guide.pdf
endif
endif


all: $(ALL)


ifneq ($(shell which gm 2> /dev/null),)
CONVERT := gm convert
else
ifneq ($(shell which convert 2> /dev/null),)
CONVERT := convert
else
CONVERT := no-convert
endif
endif

%-16.png : %.svg
	$(CONVERT) $< -resize 16x16 $@
%-32.png : %.svg
	$(CONVERT) $< -resize 32x32 $@
%-64.png : %.svg
	$(CONVERT) $< -resize 64x64 $@
%-128.png : %.svg
	$(CONVERT) $< -resize 128x128 $@
%-256.png : %.svg
	$(CONVERT) $< -resize 256x256 $@
%-512.png : %.svg
	$(CONVERT) $< -resize 512x512 $@
%-1024.png : %.svg
	$(CONVERT) $< -resize 1024x1024 $@

%-16.pdf : %.svg
	$(CONVERT) $< -resize 16x16 $@
%-32.pdf : %.svg
	$(CONVERT) $< -resize 32x32 $@
%-64.pdf : %.svg
	$(CONVERT) $< -resize 64x64 $@
%-128.pdf : %.svg
	$(CONVERT) $< -resize 128x128 $@
%-256.pdf : %.svg
	$(CONVERT) $< -resize 256x256 $@
%-512.pdf : %.svg
	$(CONVERT) $< -resize 512x512 $@
%-1024.pdf : %.svg
	$(CONVERT) $< -resize 1024x1024 $@


conf/specialsections.conf : $(ASCIIDOC_CONF_DIR)/lang-en.conf bin/mk-specialsections-conf.sh
	./bin/mk-specialsections-conf.sh $< $@

conf/html5-header.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-header-conf.sh
	./bin/mk-html5-header-conf.sh $< $@

conf/html5-footer.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-footer-conf.sh
	./bin/mk-html5-footer-conf.sh $< $@


src/Index.adoc : bin/mk-index.sh $(foreach page,$(SRC_PAGES),src/$(page).adoc)
	./bin/mk-index.sh $(SRC_PAGES) > $@

src/mlton-guide.adoc : bin/mk-mlton-guide.sh $(foreach page,$(SRC_PAGES),src/$(page).adoc)
	./bin/mk-mlton-guide.sh $(SRC_PAGES) > $@


$(ASCIIDOC_CONF_DIR)/stylesheets/asciidoc.css: ;
%/asciidoc.css : $(ASCIIDOC_CONF_DIR)/stylesheets/asciidoc.css
	mkdir -p $(dir $@) ; cp $< $@

$(ASCIIDOC_CONF_DIR)/javascripts/asciidoc.js: ;
%/asciidoc.js : $(ASCIIDOC_CONF_DIR)/javascripts/asciidoc.js
	mkdir -p $(dir $@) ; cp $< $@

$(ASCIIDOC_CONF_DIR)/stylesheets/pygments.css: ;
%/pygments.css : $(ASCIIDOC_CONF_DIR)/stylesheets/pygments.css
	mkdir -p $(dir $@) ; cp $< $@

conf/mlton.css: ;
%/mlton.css : conf/mlton.css
	mkdir -p $(dir $@) ; cp $< $@

conf/mlton-gcse.js: ;
%/mlton-gcse.js : conf/mlton-gcse.js
	mkdir -p $(dir $@) ; cp $< $@

%/index.html : %/Home
	rm -f $@ ; mkdir -p $(dir $@) ; ln -s Home $@ ; touch $@


www.mlton.org/% : src/%.adoc $(ASCIIDOC_HTML5_DEPS) bin/mk-git-attribute-entity-flags.sh
	mkdir -p $(dir $@) ; $(ASCIIDOC) $(ASCIIDOC_HTML5_FLAGS) $(shell ./bin/mk-git-attribute-entity-flags.sh $<) -a mlton-guide-host=www.mlton.org -a mlton-guide-page=$(basename $(notdir $<)) -o $@ $<

www.mlton.org/% : src/%
	mkdir -p $(dir $@) ; cp -prf $< $@

.PHONY: www.mlton.org-rm
www.mlton.org-rm: $(MLTON_ORG)
	@rm -rf $(MLTON_ORG_TORM)

.PHONY: www.mlton.org
www.mlton.org: $(MLTON_ORG)

.PHONY: upload_www.mlton.org
upload_www.mlton.org: $(MLTON_ORG) www.mlton.org-rm
	rsync -avzP --delete -e ssh www.mlton.org/ fluet,mlton@web.sourceforge.net:htdocs/wiki


localhost/% : src/%.adoc $(ASCIIDOC_HTML5_DEPS)
	mkdir -p $(dir $@) ; $(ASCIIDOC) $(ASCIIDOC_HTML5_FLAGS) -a mlton-guide-host=localhost -a mlton-guide-page=$(basename $(notdir $<)) -o $@ $<

localhost/% : src/%
	mkdir -p $(dir $@) ; cp -prf $< $@

.PHONY: localhost-rm
localhost-rm: $(LOCALHOST)
	@rm -rf $(LOCALHOST_TORM)

.PHONY: localhost
localhost: $(LOCALHOST) localhost-rm

src/mlton-guide.xml : src/mlton-guide.adoc conf/asciidoc-docbook45.flags $(ASCIIDOC_DOCBOOK45_DEPS)
	mkdir -p $(dir $@) ; $(ASCIIDOC) $(ASCIIDOC_DOCBOOK45_FLAGS) -o $@ $<
	@true || xmllint --nonet --noout --valid $@ || true
mlton-guide.pdf : src/mlton-guide.xml src/Logo.attachments/mlton-128.pdf conf/mlton-dblatex.xsl conf/mlton-dblatex.sty
	$(DBLATEX) -t pdf -p conf/mlton-dblatex.xsl -s conf/mlton-dblatex.sty -o $@ $<


.PHONY: check-git-links
check-git-links:
	@./bin/check-git-links.sh $(SRC_PAGES)

.PHONY: clean
clean:
	../../bin/clean

.PHONY: vars
vars:
	@echo ASCIIDOC_CONF_DIR=$(ASCIIDOC_CONF_DIR)
	@echo ASCIIDOC_HTML5_FLAGS=$(ASCIIDOC_HTML5_FLAGS)
	@echo ASCIIDOC_HTML5_DEPS=$(ASCIIDOC_HTML5_DEPS)
	@echo GEN_PAGES=$(GEN_PAGES)
	@echo GEN_ATTACHMENTS=$(GEN_ATTACHMENTS)
	@echo SRC_PAGES=$(SRC_PAGES)
	@echo SRC_ATTACHMENTS=$(SRC_ATTACHMENTS)
