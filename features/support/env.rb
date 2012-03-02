puts 'Running env'

$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/out/production/Qface"
$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/java"

$CLASSPATH << "/Users/gilesb/programming/JRubyProjects/qface/out/production/Qface"
$CLASSPATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/java"

#$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/ruby"

require 'java'
require "jemmy-2.2.7.5.jar"

#require "junit-4.10.jar"
#require "uispec4j-2.4-jdk16.jar"
require "monkeybars-1.1.1.jar"
require "forms_rt.jar"

#require 'aruba/cucumber'
#require 'swinger'

#java_import org.uispec4j.UISpec4J

#UISpec4J.init()



ENV['PATH'] = "#{File.expand_path(File.dirname(__FILE__) + '/../../bin')}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")