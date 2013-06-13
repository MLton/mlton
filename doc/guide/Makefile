export LC_ALL = C

NULL :=

ASCIIDOC := asciidoc
ASCIIDOC_CONF_DIR := $(shell $(ASCIIDOC) -v /dev/null 2>&1 | head -n 1 | sed 's|.*reading: \(.*\)/asciidoc.conf|\1|')
ASCIIDOC_MLTON_FLAGS := $(shell cat conf/asciidoc-mlton.flags | sed 's|^\#.*||')
ASCIIDOC_MLTON_DEPS := bin/InclGitFile.py $(shell cat conf/asciidoc-mlton.flags | sed -n 's|^.*\(conf/.*\)$$|\1|p')
ASCIIDOC_HTML5_FLAGS := $(ASCIIDOC_MLTON_FLAGS) $(shell cat conf/asciidoc-html5.flags | sed 's|^\#.*||')
ASCIIDOC_HTML5_DEPS := $(ASCIIDOC_MLTON_DEPS) $(shell cat conf/asciidoc-html5.flags | sed -n 's|^.*\(conf/.*\)$$|\1|p')

GEN_PAGES := Index
GEN_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png)

SRC_PAGES := $(filter-out $(GEN_PAGES),$(patsubst ./%.txt,%,$(shell cd txt ; find . -type f -name '*.txt')))
SRC_ATTACHMENTS := $(filter-out $(GEN_ATTACHMENTS),$(patsubst ./%,%,$(foreach dir,$(shell cd txt ; find . -type d -name '*.attachments'),$(shell cd txt ; find $(dir) -type f ! -name '.gitignore' ! -name '.gitattributes'))))

HTML5_XTRA := asciidoc.css asciidoc.js pygments.css mlton.css mlton-gcse.js index.html
HTML5_XTRA_PAGES := Index

MLTON_ORG_XTRA := $(HTML5_XTRA)
MLTON_ORG_XTRA_PAGES := $(HTML5_XTRA_PAGES)
MLTON_ORG_XTRA_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png)
MLTON_ORG := $(addprefix www.mlton.org/, $(SRC_PAGES) $(SRC_ATTACHMENTS) $(MLTON_ORG_XTRA) $(MLTON_ORG_XTRA_PAGES) $(MLTON_ORG_XTRA_ATTACHMENTS))

LOCALHOST_XTRA := $(HTML5_XTRA)
LOCALHOST_XTRA_PAGES := $(HTML5_XTRA_PAGES)
LOCALHOST_XTRA_ATTACHMENTS := $(foreach size,16 32 64 128 256 512 1024,Logo.attachments/mlton-$(size).png)
LOCALHOST := $(addprefix localhost/, $(SRC_PAGES) $(SRC_ATTACHMENTS) $(LOCALHOST_XTRA) $(LOCALHOST_XTRA_PAGES) $(LOCALHOST_XTRA_ATTACHMENTS))

ALL :=
ifneq ($(shell which $(ASCIIDOC)),)
ALL += $(LOCALHOST)
endif


all: $(ALL)


%-16.png : %.svg
	gm convert $< -resize 16x16 $@
%-32.png : %.svg
	gm convert $< -resize 32x32 $@
%-64.png : %.svg
	gm convert $< -resize 64x64 $@
%-128.png : %.svg
	gm convert $< -resize 128x128 $@
%-256.png : %.svg
	gm convert $< -resize 256x256 $@
%-512.png : %.svg
	gm convert $< -resize 512x512 $@
%-1024.png : %.svg
	gm convert $< -resize 1024x1024 $@


conf/specialsections.conf : $(ASCIIDOC_CONF_DIR)/lang-en.conf bin/mk-specialsections-conf.sh
	./bin/mk-specialsections-conf.sh $< $@

conf/html5-header.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-header-conf.sh
	./bin/mk-html5-header-conf.sh $< $@

conf/html5-footer.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-footer-conf.sh
	./bin/mk-html5-footer-conf.sh $< $@


txt/Index.txt : bin/mk-index.sh $(foreach page,$(SRC_PAGES),txt/$(page).txt)
	./bin/mk-index.sh $(SRC_PAGES) > $@


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


www.mlton.org/% : txt/%.txt conf/asciidoc-html5.flags $(ASCIIDOC_HTML5_DEPS)
	mkdir -p $(dir $@) ; $(ASCIIDOC) $(ASCIIDOC_HTML5_FLAGS) -a target=www.mlton.org -a page=$(basename $(notdir $<)) -o $@ $<

www.mlton.org/% : txt/%
	mkdir -p $(dir $@) ; cp -prf $< $@

.PHONY: www.mlton.org
www.mlton.org: $(MLTON_ORG)

.PHONY: upload_www.mlton.org
upload_www.mlton.org: $(MLTON_ORG)
	rsync -avzP --delete -e ssh www.mlton.org/ fluet,mlton@web.sourceforge.net:htdocs/wiki


localhost/% : txt/%.txt conf/asciidoc-html5.flags $(ASCIIDOC_HTML5_DEPS)
	mkdir -p $(dir $@) ; $(ASCIIDOC) $(ASCIIDOC_HTML5_FLAGS) -a target=localhost -a page=$(basename $(notdir $<)) -o $@ $<

localhost/% : txt/%
	mkdir -p $(dir $@) ; cp -prf $< $@

.PHONY: localhost
localhost: $(LOCALHOST)


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
