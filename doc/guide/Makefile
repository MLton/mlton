ROOT := ../..
include $(ROOT)/Makefile.config

######################################################################

export LC_ALL = C

NULL :=

RUBY := ruby

ASCIIDOCTOR := asciidoctor
ASCIIDOCTOR_FLAGS := --verbose
ASCIIDOCTOR_FLAGS += --attribute=mlton-version=$(MLTON_VERSION)
ASCIIDOCTOR_FLAGS += --attribute=allow-uri-read
ASCIIDOCTOR_FLAGS += --attribute=source-highlighter=rouge
ROUGE_STYLE := github
ASCIIDOCTOR_FLAGS += --attribute=rouge-style=$(ROUGE_STYLE)
ASCIIDOCTOR_HTML5_FLAGS := $(ASCIIDOCTOR_FLAGS)
# ASCIIDOCTOR_HTML5_FLAGS += --attribute=source-highlighter=highlight.js
# HIGHLIGHTJS_LANGUAGES := sml
# ASCIIDOCTOR_HTML5_FLAGS += --attribute=highlightjs-languages=$(HIGHLIGHTJS_LANGUAGES)
ASCIIDOCTOR_HTML5_FLAGS += --backend html5
ASCIIDOCTOR_HTML5_FLAGS += -r ./lib/preprocess-docinfoprocessor.rb
ASCIIDOCTOR_HTML5_FLAGS += --attribute=docinfo=shared
ASCIIDOCTOR_HTML5_FLAGS += --attribute=docinfodir="../conf/html5"
# ASCIIDOCTOR_HTML5_FLAGS += --attribute=noheader
ASCIIDOCTOR_HTML5_FLAGS += --attribute=nofooter
ASCIIDOCTOR_HTML5_FLAGS += --attribute=outfilesuffix=
ASCIIDOCTOR_HTML5_FLAGS += --attribute=relfilesuffix=
ASCIIDOCTOR_HTML5_FLAGS += --attribute=linkcss
ASCIIDOCTOR_HTML5_FLAGS += --attribute=copycss!
ASCIIDOCTOR_HTML5_DEPS := conf/html5/docinfo conf/html5/docinfo-header.inc conf/html5/docinfo-footer.inc

LOGO_SIZES := 16 32 64 128 256 512 1024

GEN_PAGES := Index mlton-guide
GEN_ATTACHMENTS := $(foreach S,$(LOGO_SIZES),Logo.attachments/mlton-$(S).png)
GEN_ATTACHMENTS += $(foreach S,$(LOGO_SIZES),Logo.attachments/mlton-$(S).pdf)

SRC_PAGES := $(filter-out $(GEN_PAGES),$(patsubst ./%.adoc,%,$(shell cd src ; $(FIND) . -type f -name '*.adoc')))
SRC_ATTACHMENTS := $(filter-out $(GEN_ATTACHMENTS),$(patsubst ./%,%,$(foreach dir,$(shell cd src ; $(FIND) . -type d -name '*.attachments'),$(shell cd src ; $(FIND) $(dir) -type f ! -name '.gitignore' ! -name '.gitattributes'))))

HTML5_XTRA := asciidoctor.css rouge-$(ROUGE_STYLE).css mlton.css index.html
HTML5_XTRA_PAGES := Index
HTML5_XTRA_ATTACHMENTS := $(foreach S,$(LOGO_SIZES),Logo.attachments/mlton-$(S).png)
HTML5_TARGETS := $(HTML5_XTRA) $(SRC_PAGES) $(SRC_ATTACHMENTS) $(HTML5_XTRA_PAGES) $(HTML5_XTRA_ATTACHMENTS)


ALL :=
ifeq (true, $(call HAVE_CMD,$(ASCIIDOCTOR)))
ALL += $(LOCALHOST) localhost-rm
endif


all: $(ALL)


ifeq (true, $(call HAVE_CMD,gm))
CONVERT := gm convert
else
ifeq (true, $(call HAVE_CMD,convert))
CONVERT := convert
else
CONVERT := no-convert
endif
endif

define MK_CONVERT_RESIZE
%-$(3).$(2) : %.$(1)
	$$(CONVERT) $$< -resize $(3)x$(3) $$@
endef
$(foreach S,$(LOGO_SIZES),$(eval $(call MK_CONVERT_RESIZE,svg,png,$S)))
$(foreach S,$(LOGO_SIZES),$(eval $(call MK_CONVERT_RESIZE,svg,pdf,$S)))



src/Index.adoc : bin/mk-index.sh $(foreach page,$(SRC_PAGES),src/$(page).adoc)
	./bin/mk-index.sh $(SRC_PAGES) > $@

src/mlton-guide.adoc : bin/mk-mlton-guide.sh $(foreach page,$(SRC_PAGES),src/$(page).adoc)
	./bin/mk-mlton-guide.sh $(SRC_PAGES) > $@


%/asciidoctor.css :
	@mkdir -p $(dir $@)
	$(RUBY) -e "require 'asciidoctor'" -e "Asciidoctor::Stylesheets.instance.write_primary_stylesheet '$(dir $@)'"
%/rouge-$(ROUGE_STYLE).css :
	@mkdir -p $(dir $@)
	$(RUBY) -e "require 'asciidoctor'" -e "rsh = Asciidoctor::SyntaxHighlighter::create 'rouge'" -e "::File.write (::File.join '$(dir $@)', (rsh.stylesheet_basename '$(ROUGE_STYLE)')), (rsh.read_stylesheet '$(ROUGE_STYLE)'), mode: Asciidoctor::FILE_WRITE_MODE"

conf/html5/mlton.css: ;
%/mlton.css : conf/html5/mlton.css
	$(MKDIR) $(dir $@) ; $(CP) $< $@


%/index.html : %/Home
	$(RM) $@ ; $(MKDIR) $(dir $@) ; ln -s Home $@ ; touch $@


define MK_HTML5
$(1)_TARGETS := $$(addprefix $(1)/, $$(HTML5_TARGETS))

$(1)/% : src/%.adoc bin/mk-git-attribute-entity-flags.sh $$(ASCIIDOCTOR_HTML5_DEPS)
	@$$(MKDIR) $$(dir $$@)
	$$(ASCIIDOCTOR) $$(ASCIIDOCTOR_HTML5_FLAGS) --attribute=mlton-guide-host=$(1) $$(shell ./bin/mk-git-attribute-entity-flags.sh $$<) -o $$@ $$<

$(1)/% : src/%
	@$$(MKDIR) $$(dir $$@)
	$$(CP) $$< $$@

.PHONY: $(1)-rm
$(1)-rm: $$($(1)_TARGETS)
	@$$(RM) $$(filter-out $$($(1)_TARGETS),$$(shell if [ -d $(1) ]; then $$(FIND) $(1) -type f; fi))

.PHONY: $(1)
$(1): $$($(1)_TARGETS) $(1)-rm
endef

$(eval $(call MK_HTML5,localhost))
$(eval $(call MK_HTML5,mlton.sourceforge.io))

.PHONY: upload_mlton.sourceforge.io
upload_mlton.sourceforge.io: mlton.sourceforge.io
	rsync -avzP --delete -e ssh mlton.sourceforge.io/ fluet,mlton@web.sourceforge.net:htdocs/wiki


######

SHOW_VARS += ASCIIDOCTOR_HTML5_FLAGS
SHOW_VARS += GEN_PAGES GEN_ATTACHMENTS SRC_PAGES SRC_ATTACHMENTS

$(eval $(MK_SHOW_CONFIG))


.PHONY: clean
clean:
	../../bin/clean
