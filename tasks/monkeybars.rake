require 'fileutils'
require_relative 'monkey_generator'

desc 'ALL, CONTROLLER, VIEW, MODEL, UI are valid options.'
task 'generate'
rule(/^generate/) do |_t|
  ARGV[1..-1].each do |generator_command|
    command, argument = generator_command.split '='
    mg = MonkeyGenerator.new(argument)
    case command
    when 'ALL'
      generate_tuple mg
    when 'VIEW'
      mg.generate_view
    when 'CONTROLLER'
      mg.generate_controller
    when 'MODEL'
      mg.generate_model
    when 'UI'
      mg.generate_ui

    else
      $stdout << "Unknown generate target #{argument}"
    end
  end
end

def generate_tuple(mgenerator)
  pwd = FileUtils.pwd
  mgenerator.generate_controller

  FileUtils.cd pwd
  mgenerator.generate_model

  FileUtils.cd pwd
  mgenerator.generate_view true

  FileUtils.cd pwd
  mgenerator.generate_ui
end

# def generate_controller(path)
#   name = setup_directory path
#   file_name = "#{name}_controller.rb"
#   name = camelize name
#   $stdout << "Generating controller #{name}Controller in file #{file_name}\n"
#   File.open(file_name, 'w') do |controller_file|
#     controller_file << <<-ENDL
#   ENDL
#   end
# end

# def generate_model(path)
#   name = setup_directory path
#   file_name = "#{name}_model.rb"
#   name = camelize name
#   $stdout << "Generating model #{name}Model in file #{file_name}\n"
#   File.open(file_name, 'w') do |model_file|
#     model_file << <<-ENDL
# class #{name}Model

# end
#   ENDL
#   end
# end

# def generate_view(path, from_all = false)
#   name = setup_directory path
#   file_name = "#{name}_view.rb"
#   cname = camelize name
#   $stdout << "Generating view #{cname}View in file #{file_name}\n"
#   ui_require = from_all ? "require '#{name}/#{name}_ui'" : ''
#   java_class = from_all ? "#{cname}Ui" : "''"
#   File.open(file_name, 'w') do |view_file|
#     view_file << <<-ENDL
# #{ui_require}

# class #{cname}View < ApplicationView
#   set_java_class #{java_class}
# end
#   ENDL
#   end
# end

# def generate_ui(path)
#   name = setup_directory path
#   file_name = "#{name}_ui.rb"
#   name = camelize name
#   $stdout << "Generating ui #{name}Ui in file #{file_name}\n"
#   File.open(file_name, 'w') do |ui_file|
#     ui_file << <<-ENDL
#   ENDL
#   end
# end

# def setup_directory(path)
#   FileUtils.mkdir_p path.gsub('\\', '/')
#   FileUtils.cd path
#   path.split('/').last
# end

# def camelize(name, first_letter_in_uppercase = true)
#   name = name.to_s
#   if first_letter_in_uppercase
#     name.gsub(/\/(.?)/) { '::' + Regexp.last_match[1].upcase }
#       .gsub(/(^|_)(.)/) { Regexp.last_match[2].upcase }
#   else
#     name[0..0] + camelize(name[1..-1])
#   end
# end
