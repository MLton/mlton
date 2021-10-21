require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include Asciidoctor

# A docinfo processor that allows docinfo content to be preprocessed.
Extensions.register do
  docinfo_processor do
    at_location :head
    process do |doc|
      ext = '.inc'
      path = doc.normalize_system_path %(docinfo#{ext}), doc.attributes['docinfodir']
      doc.sub_attributes (PreprocessorReader.new doc, doc.read_asset(path)).read
    end
  end
end

Extensions.register do
  docinfo_processor do
    at_location :header
    process do |doc|
      ext = '.inc'
      path = doc.normalize_system_path %(docinfo-header#{ext}), doc.attributes['docinfodir']
      doc.sub_attributes (PreprocessorReader.new doc, doc.read_asset(path)).read
    end
  end
end

Extensions.register do
  docinfo_processor do
    at_location :footer
    process do |doc|
      ext = '.inc'
      path = doc.normalize_system_path %(docinfo-footer#{ext}), doc.attributes['docinfodir']
      doc.sub_attributes (PreprocessorReader.new doc, doc.read_asset(path)).read
    end
  end
end
