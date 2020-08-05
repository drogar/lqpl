class ImportJava
  def self.do_imports(context:, awt: [], lang: [], swing: [])
    import_awt(context, awt)
    import_lang(context, lang)
    import_swing(context, swing)
  end

  # TODO: Use meta programming to create these
  def self.import_awt(context, files)
    import_package(context, :awt, Array(files))
  end

  def self.import_swing(context, files)
    import_package(context, :swing, Array(files))
  end

  def self.import_lang(context, files)
    import_package(context, :lang, Array(files))
  end

  def self.import_package(context, package, files)
    top = package == :swing ? 'javax' : 'java'
    files.each do |file|
      context.instance_eval { java_import "#{top}.#{package}.#{file}" }
    end
  end
end
