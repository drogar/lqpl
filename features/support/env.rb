require 'simplecov'
SimpleCov.start

if not (defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby')
  abort 'Sorry - Feature tests of LQPL requires JRuby. You appear to be running or defaulted to some other ruby engine.'
end

$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/src"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../out/lqpl_gui"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/lib/java"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/devlib/java"
$LOAD_PATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/lib/ruby"

require 'java'

$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../out/lqpl_gui"


$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/lib/java/jruby-complete.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/devlib/java/jemmy-2.3.0.0.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/lib/java/forms_rt.jar"
$CLASSPATH << File.expand_path(File.dirname(__FILE__))+"/../../GUI/lib/java/monkeybars-1.1.1.jar"


require "fest-swing-1.2.jar"

require "monkeybars-1.1.1.jar"
require "forms_rt.jar"

ENV['PATH'] = "#{File.expand_path(File.dirname(__FILE__) + '/../../out/bin')}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
java.lang.System.set_property("com.drogar.testing.jemmy","true")

require 'manifest'

java_import org.fest.swing.edt.GuiActionRunner
java_import org.fest.swing.edt.GuiQuery
java_import org.fest.swing.fixture.JMenuItemFixture
java_import org.fest.swing.fixture.FrameFixture
java_import org.fest.swing.core.matcher.JButtonMatcher
java_import org.fest.swing.core.matcher.JLabelMatcher
#java_import org.netbeans.jemmy.JemmyProperties
#java_import org.netbeans.jemmy.TestOut

# ["JFileChooserOperator","Operator","JButtonOperator","JLabelOperator","ContainerOperator",
#   "JSpinnerOperator","JTabbedPaneOperator","JTextAreaOperator","JFrameOperator",
#   "JDialogOperator","JMenuBarOperator","JMenuOperator","JMenuItemOperator"].each do |c|
#     java_import "org.netbeans.jemmy.operators."+c
# end
# 
# java_import org.netbeans.jemmy.drivers.menus.AppleMenuDriver



#  java_import org.netbeans.jemmy.Timeouts
#  btn_timeout = Timeouts.new
#  btn_timeout.setTimeout("ComponentOperator.WaitComponentTimeout", 100)

java_import javax.swing.JButton


# props = JemmyProperties.properties
# JemmyProperties.current_keys.each {|k| puts "Prop: #{k}    =  #{JemmyProperties.get_current_property(k)}"}

# testout - (in, trace out, error out, notes out)
# JemmyProperties.set_current_output(TestOut.new(java.lang.System.in, nil, java.lang.System.err, nil))

# amd = AppleMenuDriver.new
#
# JemmyProperties.set_current_property("drivers.menu.org.netbeans.jemmy.operators.JMenuOperator",amd)
# JemmyProperties.set_current_property("drivers.menu.org.netbeans.jemmy.operators.JMenuBarOperator",amd)

class AppStarter < GuiTask
  # Launch the app in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  # FEST will call this method for us before the test.
  #
  def executeInEDT
    com.drogar.lqpl.Main.main([])
  end
end

Before do
  runner = GuiActionRunner.execute(AppStarter.new)
  @qe_frame = FrameFixture.new(runner)
end


After do
  title = 'Confirm Exit - PresentationClock'
  @window.close
  @window.option_pane.require_title(title).yes_button.click
end

# begin
#   puts "Starting up!!!!"
#   com.drogar.lqpl.Main.main([])
# rescue Exception => e
#   puts "Exception from main: #{e}"
# end
# 
# $qe_frame = JFrameOperator.new "Quantum Emulator"

at_exit {
  
  LqplController.instance.close
 # $qe_frame.close
 
}