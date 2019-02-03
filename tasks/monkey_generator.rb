# Abstract out the generators for monkeybars, use template files
class MonkeyGenerator
  attr_accessor :name
  attr_accessor :camel_name

  def initialize(iname)
    @templates = {}
    %i[controller model ui view].each do |template_type|
      @templates[template_type] = File.read("tasks/mb_#{template_type}.template")
    end
    self.name = setup_directory iname
    self.camel_name = camelize name
  end

  def file_name(type)
    "#{name}_#{type}.rb"
  end

  def generate_controller
    fname = file_name(:controller)
    $stdout << "Generating controller #{camel_name}Controller in file #{fname}\n"
    File.open(fname, 'w') do |controller_file|
      controller_file << format(@templates[:controller], camel_name, camel_name, camel_name)
    end
  end

  def generate_view(from_all = false)
    fname = file_name(:view)
    $stdout << "Generating view #{camel_name}View in file #{fname}\n"
    ui_require = from_all ? "require '#{name}/#{name}_ui'" : ''
    java_class = from_all ? "#{camel_name}Ui" : "''"
    File.open(fname, 'w') do |view_file|
      view_file << format(@templates[:view], ui_require, camel_name, java_class)
    end
  end

  def generate_ui
    fname = file_name(:ui)
    $stdout << "Generating ui #{camel_name}Ui in file #{fname}\n"
    File.open(fname, 'w') do |ui_file|
      ui_file << format(@templates[:ui], camel_name)
    end
  end

  def generate_model
    fname = file_name(:model)
    $stdout << "Generating model #{camel_name}Model in file #{fname}\n"
    File.open(fname, 'w') do |model_file|
      model_file << format(@templates[:model], camel_name)
    end
  end

  def setup_directory(path)
    FileUtils.mkdir_p path.tr('\\', '/')
    FileUtils.cd path
    path.split('/').last
  end

  def camelize(first_letter_in_uppercase = true)
    cname = name.to_s
    return cname[0..0] + camelize(cname[1..-1]) unless first_letter_in_uppercase

    sub_separator_and_upcase(cname).gsub(/(^|_)(.)/) { Regexp.last_match[2].upcase }
  end

  def sub_separator_and_upcase(cname)
    cname.gsub(%r{/(.?)}) { '::' + Regexp.last_match[1].upcase }
  end
end
