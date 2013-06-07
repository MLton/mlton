export LC_ALL = C

NULL :=

ASCIIDOC := asciidoc
ASCIIDOCFLAGS := $(shell cat conf/asciidoc.flags | sed 's|^\#.*||')
ASCIIDOC_CONF_DIR := $(shell asciidoc -v /dev/null 2>&1 | head -n 1 | sed 's|.*reading: \(.*\)/asciidoc.conf|\1|')

SRC_PAGES := $(patsubst ./%.txt,%,$(shell cd txt ; find . -type f -name '*.txt'))
SRC_ATTACHMENTS := $(patsubst ./%,%,$(foreach dir,$(shell cd txt ; find . -type d -name '*.attachments'),$(shell cd txt ; find $(dir) -type f ! -name '.gitignore' ! -name '.gitattributes')))

WIKI_XTRA_PAGES := Index
WIKI_XTRA_ATTACHMENTS := $(foreach size,32 64 128 256 512 1024,Logo.attachments/mlton.$(size).png)
WIKI := $(addprefix wiki/,asciidoc.css asciidoc.js pygments.css mlton_org.css mlton_org-gcse.js index.html $(SRC_PAGES) $(SRC_ATTACHMENTS) $(WIKI_XTRA_PAGES) $(WIKI_XTRA_ATTACHMENTS))

ALL :=
ifneq ($(shell which asciidoc),)
ALL += $(WIKI)
endif


all: $(ALL)


conf/html5-header.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-header-conf.sh
	./bin/mk-html5-header-conf.sh $< $@

conf/html5-footer.conf : $(ASCIIDOC_CONF_DIR)/html5.conf bin/mk-html5-footer-conf.sh
	./bin/mk-html5-footer-conf.sh $< $@


wiki/asciidoc.css : $(ASCIIDOC_CONF_DIR)/stylesheets/asciidoc.css
	mkdir -p $(dir $@) ; cp $< $@

wiki/asciidoc.js : $(ASCIIDOC_CONF_DIR)/javascripts/asciidoc.js
	mkdir -p $(dir $@) ; cp $< $@

wiki/pygments.css : $(ASCIIDOC_CONF_DIR)/stylesheets/pygments.css
	mkdir -p $(dir $@) ; cp $< $@

wiki/mlton_org.css : conf/mlton_org.css
	mkdir -p $(dir $@) ; cp $< $@

wiki/mlton_org-gcse.js : conf/mlton_org-gcse.js
	mkdir -p $(dir $@) ; cp $< $@

wiki/index.html : wiki/Home
	rm -f $@ ; mkdir -p $(dir $@) ; ln -s Home $@ ; touch $@

define WIKI_PAGE_template
wiki/$(1) : txt/$(1).txt conf/asciidoc.flags conf/mlton_org-asciidoc.conf conf/html5-header.conf conf/html5-footer.conf conf/mlton_org-html5.conf bin/InclGitFile.py
	mkdir -p $$(dir $$@) ; $$(ASCIIDOC) $$(ASCIIDOCFLAGS) -o $$@ $$<
endef
$(foreach page,$(SRC_PAGES),$(eval $(call WIKI_PAGE_template,$(page))))

define WIKI_ATTACHMENT_template
wiki/$(1) : txt/$(1)
	mkdir -p $$(dir $$@) ; cp -prf $$< $$@
endef
$(foreach attachment,$(SRC_ATTACHMENTS),$(eval $(call WIKI_ATTACHMENT_template,$(attachment))))

wiki/Index : bin/mk-index.sh $(foreach page,$(SRC_PAGES),txt/$(page).txt) conf/asciidoc.flags conf/mlton_org-asciidoc.conf conf/html5-header.conf conf/html5-footer.conf conf/mlton_org-html5.conf bin/InclGitFile.py
	mkdir -p $(dir $@) ; ./bin/mk-index.sh $(SRC_PAGES) | $(ASCIIDOC) $(ASCIIDOCFLAGS) -o $@ -

define SVG2PNG_template
%.$(1).png : %.svg
	gm convert $$< -resize $(1)x$(1) $$@
endef
$(foreach size,32 64 128 256 512 1024,$(eval $(call SVG2PNG_template,$(size))))

.PHONY: upload_wiki
upload_wiki: $(WIKI)
	rsync -avzP --delete -e ssh wiki/ fluet,mlton@web.sourceforge.net:htdocs/wiki


.PHONY: clean
clean:
	../../bin/clean

.PHONY: vars
vars:
	@echo ASCIIDOC_CONF_DIR=$(ASCIIDOC_CONF_DIR)
	@echo SRC_PAGES=$(SRC_PAGES)
	@echo SRC_ATTACHMENTS=$(SRC_ATTACHMENTS)
	@echo XTRA_ATTACHMENTS=$(XTRA_ATTACHMENTS)
	@echo WIKI=$(WIKI)
