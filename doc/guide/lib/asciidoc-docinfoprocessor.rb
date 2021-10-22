require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include Asciidoctor

# A docinfo processor that allows docinfo content to be written in
# AsciiDoc format. The AsciiDoc file is converted in the same manner
# as an AsciiDoc table cell.
Extensions.register do
  docinfo_processor do
    at_location :head
    process do |doc|
      ext = ::File.extname doc.attributes['docfile']
      path = doc.normalize_system_path %(docinfo#{ext}), doc.attributes['docinfodir']
      ::Asciidoctor.convert doc.read_asset(path), parent: doc
    end
  end
end

Extensions.register do
  docinfo_processor do
    at_location :header
    process do |doc|
      ext = ::File.extname doc.attributes['docfile']
      path = doc.normalize_system_path %(docinfo-header#{ext}), doc.attributes['docinfodir']
      ::Asciidoctor.convert doc.read_asset(path), parent: doc
    end
  end
end

Extensions.register do
  docinfo_processor do
    at_location :footer
    process do |doc|
      ext = ::File.extname doc.attributes['docfile']
      path = doc.normalize_system_path %(docinfo-footer#{ext}), doc.attributes['docinfodir']
      ::Asciidoctor.convert doc.read_asset(path), parent: doc
    end
  end
end
