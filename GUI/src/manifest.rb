# encoding: utf-8
require 'rbconfig'
require 'java'
require 'config/platform'
require 'json'
require 'config/pathing'
require 'config/resolver'
require 'config/import_java'
require 'pry'

Pathing.add_all_sibling_directories_to_load_path(__FILE__)
# for swinging
Pathing.add_to_load_path '../lib/ruby/swingtown'

Monkeybars::Resolver.new(location: __FILE__).add_monkeybars_jar_when_in_file_system

require 'monkeybars'
require 'utility/swing_runner'
require 'controller/application_controller'
require 'view/application_view'

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
#
# TODO: Write a hook methods to handle this
# case Monkeybars::Resolver.run_location
# when Monkeybars::Resolver::IN_FILE_SYSTEM
#   # Files to be added only when running from the file system go here
# when Monkeybars::Resolver::IN_JAR_FILE
#   # Files to be added only when run from inside a jar file
# end

ImportJava.do_imports(context: self,
                      awt: %w(BorderLayout GridLayout Point Rectangle),
                      swing: %w(JOptionPane JFileChooser filechooser.FileNameExtensionFilter
                                JTextArea JScrollPane BoxLayout SpinnerNumberModel),
                      lang: 'System')

# for swinging
require 'swingtown'
include Swingtown::Core

# end for swinging
require 'model/application_model'

require 'controller/lqpl_controller'
