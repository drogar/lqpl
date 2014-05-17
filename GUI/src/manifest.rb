# encoding: utf-8
require 'rbconfig'
require 'java'
require 'config/platform'
require 'json'

Dir.glob(File.expand_path(File.dirname(__FILE__) +
  '/**/*').gsub('%20', ' ')).each do |directory|
  # File.directory? is broken in current JRuby for dirs inside jars
  # http://jira.codehaus.org/browse/JRUBY-2289
  $LOAD_PATH << directory unless directory =~ /\.\w+$/
end

# puts "set loadpath #{$LOAD_PATH}"
# Some JRuby $LOAD_PATH path bugs to check if you're having trouble:
# http://jira.codehaus.org/browse/JRUBY-2518 -
#         Dir.glob and Dir[] doesn't work
#         for starting in a dir in a jar
#         (such as Active-Record migrations)
# http://jira.codehaus.org/browse/JRUBY-3247 -
#         Compiled Ruby classes produce
#         word substitutes for characters
#         like - and . (to minus and dot).
#         This is problematic with gems
#         like ActiveSupport and Prawn

# ===================================================================
# Monkeybars requires, this pulls in the requisite libraries needed
# for Monkeybars to operate.

require 'resolver'
#:nocov:
def monkeybars_jar(path)
  Dir.glob(path).select { |f| f =~ /(monkeybars-)(.+).jar$/ }.first
end

case Monkeybars::Resolver.run_location
when Monkeybars::Resolver::IN_FILE_SYSTEM
  here = File.expand_path File.dirname(__FILE__)
  #    npath = here + '/../lib/java/*.jar'
  #    puts npath
  mbj =   monkeybars_jar(here + '/../lib/java/*.jar')
  #    puts "resulting mbj=#{mbj}"
  if !mbj || mbj == ''
    mbj =  monkeybars_jar(here + '/../../../lib/java/*.jar')
    #     puts "or is #{mbj}"
  end
  add_to_classpath mbj
end
#:nocov:

require 'monkeybars'

require 'application_controller'
require 'application_view'

# End of Monkeybars requires
# ==================================================================
#
# Add your own application-wide libraries below.  To include jars,
# append to $CLASSPATH, or use add_to_classpath, for example:
#
# $CLASSPATH << File.expand_path(File.dirname(__FILE__) +
#    "/../lib/java/swing-layout-1.0.3.jar")
#
# is equivalent to
#
# add_to_classpath "../lib/java/swing-layout-1.0.3.jar"
#
# There is also a helper for adding to your load path and avoiding
# issues with file: being appended to the load path (useful for
# JRuby libs that need your jar directory on the load path).
#
# add_to_load_path "../lib/java"
#

case Monkeybars::Resolver.run_location
when Monkeybars::Resolver::IN_FILE_SYSTEM
  # Files to be added only when running from the file system go here
when Monkeybars::Resolver::IN_JAR_FILE
  # Files to be added only when run from inside a jar file
end

%w(BorderLayout GridLayout).each do |awtfile|
  java_import "java.awt.#{awtfile}"
end

%w(JOptionPane JFileChooser filechooser.FileNameExtensionFilter
   JTextArea JScrollPane BoxLayout SpinnerNumberModel).each do |cfile|
  java_import 'javax.swing.' + cfile
end

# for swinging
add_to_load_path '../lib/ruby/swingtown'
require 'swingtown'
include Swingtown::Core

# end for swinging

java_import java.lang.System

java_import java.awt.Point

%w(point jfile_chooser array).each do |rfile|
  require 'utility/monkey/' + rfile
end

%w(drawing duck_matcher ensure_json swing_runner parameter_generator).each do |f|
  require 'utility/' + f
end

%w(abstract_pattern zero_pattern value_pattern abstract_list_pattern
   qubit_pattern data_pattern classical_pattern stack_translation
   code_pointer executing_code dump_call dump_split dump
   quantum_stack).each do |rf|
  require 'panels/parsers/' + rf + '_parser'
end

require 'dialogs/parsers/simulate_results_parser'
require 'painting/canvas_size'
require 'application_model'
require 'panels/panel_controller'

%w(server_process_not_found invalid_input).each do |f|
  require 'exceptions/' + f
end

%w(lqpl_emulator_server_connection compiler_command_interpretor
   emulator_command_interpretor compiler_server_connection).each do |f|
  require 'communications/' + f
end

%w(about simulate_results).each do |dialog|
  require "forms/dialogs/#{dialog}_dialog"
end

%w(quantum_stack_panel).each do |component|
  require "forms/components/#{component}"
end

%w(scrollable_label).each do |generic_form|
  require "forms/generic/#{generic_form}"
end

%w(classical_stack dump executing_code quantum_emulator_main
   quantum_stack stack_translation).each do |a_form|
  require "forms/#{a_form}_form"
end

%w(abstract classical data qubit value zero).each do |rf|
  require "panels/quantum_stack/descriptor/#{rf}_descriptor_model"
  require "panels/quantum_stack/descriptor/#{rf}_descriptor_painter"
end

require 'panels/quantum_stack/descriptor/descriptor_painter_factory'
require 'panels/quantum_stack/quantum_stack_painter'

# %w(classical_stack dump executing_code quantum_stack stack_translation).each do |a_panel|
#   require "panels/#{a_panel}/#{a_panel}_view"
#   require "panels/#{a_panel}/#{a_panel}_model"
#   require "panels/#{a_panel}/#{a_panel}_controller"
# end

# SwingRunner::on_edt do
require 'lqpl_menu'
require 'lqpl_subs_handler'
{ 'panels/' => %w(quantum_stack classical_stack dump executing_code
                  stack_translation),
  '' => %w(lqpl),
  'dialogs/' => %w(simulate_results about) }.each do |k, v|
  v.each do |f|
    require k + f + '/' + f + '_view'
    require k + f + '/' + f + '_model'
    require k + f + '/' + f + '_controller'
  end
end
# end

require 'exit_handler'
