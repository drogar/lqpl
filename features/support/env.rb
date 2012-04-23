puts 'Running env'

#$LOAD_PATH << "./out/production/Qface"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../out/production/Qface"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/ruby"
#$LOAD_PATH << "./qface/lib/java"


#$LOAD_PATH << "/Users/gilesb/programming/JRubyProjects/qface/lib/ruby"

require 'java'

$CLASSPATH << "./out/production/Qface"
$CLASSPATH << "./qface/lib/java"


$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../src"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java/jemmy-2.2.7.5.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java/miglayout-3.7.3.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../out/production/Qface"

puts $CLASSPATH

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