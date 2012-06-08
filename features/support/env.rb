
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../out/production/Qface"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/java"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../lib/ruby"

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


require "jemmy-2.2.7.5.jar"

#require "junit-4.10.jar"
#require "uispec4j-2.4-jdk16.jar"
require "monkeybars-1.1.1.jar"
require "forms_rt.jar"

ENV['PATH'] = "#{File.expand_path(File.dirname(__FILE__) + '/../../bin')}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")

require 'manifest'

java_import org.netbeans.jemmy.JemmyProperties
java_import org.netbeans.jemmy.TestOut

java_import org.netbeans.jemmy.operators.JFileChooserOperator
java_import org.netbeans.jemmy.operators.Operator

java_import org.netbeans.jemmy.operators.JButtonOperator
java_import org.netbeans.jemmy.operators.JLabelOperator
java_import org.netbeans.jemmy.operators.ContainerOperator
java_import org.netbeans.jemmy.operators.JSpinnerOperator
java_import org.netbeans.jemmy.operators.JTabbedPaneOperator
java_import org.netbeans.jemmy.operators.JTextAreaOperator
java_import org.netbeans.jemmy.operators.JFrameOperator
java_import org.netbeans.jemmy.operators.JMenuBarOperator
java_import org.netbeans.jemmy.operators.JMenuOperator
java_import org.netbeans.jemmy.operators.JMenuItemOperator



#  java_import org.netbeans.jemmy.Timeouts
#  btn_timeout = Timeouts.new
#  btn_timeout.setTimeout("ComponentOperator.WaitComponentTimeout", 100)

java_import javax.swing.JButton


#props = JemmyProperties.properties
#JemmyProperties.current_keys.each {|k| puts "Prop: #{k}    =  #{JemmyProperties.get_current_property(k)}"}

# testout - (in, trace out, error out, notes out)
JemmyProperties.set_current_output(TestOut.new(java.lang.System.in, nil, java.lang.System.err, nil))

begin
  puts "Starting up!!!!"
  com.drogar.qface.Main.main([])
rescue Exception => e
  puts "Exception from main: #{e}"
end

java_import org.netbeans.jemmy.operators.JFrameOperator
$qe_frame = JFrameOperator.new "Quantum Emulator"

at_exit {
  $qe_frame.close
  QfaceController.instance.close}