$LOAD_PATH << __dir__ + '/lib/java'
$LOAD_PATH << __dir__ + '/lib/ruby'
$LOAD_PATH << __dir__ + '/devlib/ruby'
# $LOAD_PATH << "./lqpl/lib/java"

# $LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/lqpl/lib/ruby"

require 'java'

$CLASSPATH << '../out/lqpl_gui/'

$LOAD_PATH << __dir__ + '/src'
$CLASSPATH << __dir__ + '/lib/java/jruby-complete.jar'
$CLASSPATH << __dir__ + '/lib/java/monkeybars-1.1.1.jar'
# $CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/out/production/lqpl_gui"

# test to see if running gives edt violations
# uncomment down to 'end testing of edt violations'
# %w{fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2
#    fest-util-1.1.2 jcip-annotations-1.0}.each do |jar|
#   $CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/devlib/java/" + jar+".jar"
# end
# puts $CLASSPATH
#
# require "devlib/java/fest-swing-1.2.jar"
#
#
# require '/Users/gilesb/programming/mixed/lqpl/GUI/devlib/ruby/fest_testing_imports'
# end testing of edt violations

require 'monkeybars-1.1.1.jar'
require 'utility/swing_runner'

ENV['PATH'] = File.expand_path(File.dirname(__FILE__) + '/bin') +
              "#{File::PATH_SEPARATOR}#{ENV['PATH']}"

begin
  com.drogar.lqpl.Main.main([''].to_java(:string))
rescue StandardError => e
  puts "Had a problem: #{e}"
end
