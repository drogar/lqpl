Dir.glob(File.expand_path(File.dirname(__FILE__) + "/**/*").gsub('%20', ' ')).each do |directory|
  # File.directory? is broken in current JRuby for dirs inside jars
  # http://jira.codehaus.org/browse/JRUBY-2289
  $LOAD_PATH << directory unless directory =~ /\.\w+$/
end


#puts "set loadpath #{$LOAD_PATH}"
# Some JRuby $LOAD_PATH path bugs to check if you're having trouble:
# http://jira.codehaus.org/browse/JRUBY-2518 - Dir.glob and Dir[] doesn't work
#                                              for starting in a dir in a jar
#                                              (such as Active-Record migrations)
# http://jira.codehaus.org/browse/JRUBY-3247 - Compiled Ruby classes produce
#                                              word substitutes for characters
#                                              like - and . (to minus and dot).
#                                              This is problematic with gems
#                                              like ActiveSupport and Prawn

#===============================================================================
# Monkeybars requires, this pulls in the requisite libraries needed for
# Monkeybars to operate.

require 'resolver'

def monkeybars_jar path
  Dir.glob(path).select { |f| f =~ /(monkeybars-)(.+).jar$/}.first
end

case Monkeybars::Resolver.run_location
  when Monkeybars::Resolver::IN_FILE_SYSTEM

    here = File.expand_path File.dirname(__FILE__)
    npath = here + '/../lib/java/*.jar'
#    puts npath
    mbj =   monkeybars_jar( here + '/../lib/java/*.jar' )
#    puts "resulting mbj=#{mbj}"
    if !mbj || mbj == ''
      mbj =  monkeybars_jar( here + '/../../../lib/java/*.jar' )
#      puts "or is #{mbj}"
    end
    add_to_classpath mbj
end


require 'monkeybars'
require 'application_controller'
require 'application_view'

# End of Monkeybars requires
#===============================================================================
#
# Add your own application-wide libraries below.  To include jars, append to
# $CLASSPATH, or use add_to_classpath, for example:
#
# $CLASSPATH << File.expand_path(File.dirname(__FILE__) + "/../lib/java/swing-layout-1.0.3.jar")
#
# is equivalent to
#
# add_to_classpath "../lib/java/swing-layout-1.0.3.jar"
#
# There is also a helper for adding to your load path and avoiding issues with file: being
# appended to the load path (useful for JRuby libs that need your jar directory on
# the load path).
#
# add_to_load_path "../lib/java"
#


case Monkeybars::Resolver.run_location
when Monkeybars::Resolver::IN_FILE_SYSTEM
  # Files to be added only when running from the file system go here
when Monkeybars::Resolver::IN_JAR_FILE
  # Files to be added only when run from inside a jar file
end

require 'painting/canvas_size'
require 'application_model'
require 'utility/monkey/array'
require 'utility/xml_decode'
require 'xml_based_model'
["server_process_not_found", "invalid_input"].each do |f|
  require "exceptions/"+f
end

["translate_line_ends","xml_decode"].each do |f|
  require "utility/"+f
end

["lqpl_emulator_server_connection","compiler_server_connection"].each do |f|
  require "communications/"+f
end

{ ""=>["lqpl"], 
  "panels/" => ["quantum_stack", "classical_stack","dump","executable_code", "stack_translation"],
  "dialogs/" =>["simulate_results", "about"]}.each do |k,v|
    v.each {|f|   require k+f+"/"+f+"_controller" }
end

require "exit_handler"

java_import javax.swing.JOptionPane
java_import java.lang.System


