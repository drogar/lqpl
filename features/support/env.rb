require 'simplecov'
SimpleCov.start

if not (defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby')
  abort 'Sorry - Feature tests of LQPL requires JRuby. You appear to be running or defaulted to some other ruby engine.'
end
where_i_am = File.expand_path(File.dirname(__FILE__))
%w{src lqpl_gui lib/java lib/ruby devlib/java}.each do |dir|
  $LOAD_PATH << where_i_am+"/../../GUI/"+ dir
end
$LOAD_PATH << where_i_am+"/../../out/lqpl_gui"


require 'java'

$CLASSPATH << where_i_am+"/../../out/lqpl_gui"

#runtime jars
%w{jruby-complete forms_rt monkeybars-1.1.1}.each do |jar|
  $CLASSPATH << where_i_am+"/../../GUI/lib/java/"+jar+".jar"
end

#testing jars
%w{fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2 fest-util-1.1.2 jcip-annotations-1.0}.each do |jar|
  $CLASSPATH << where_i_am+"/../../GUI/devlib/java/" + jar+".jar"
end


require "fest-swing-1.2.jar"

require "monkeybars-1.1.1.jar"
require "forms_rt.jar"

ENV['PATH'] = "#{where_i_am + '/../../out/bin'}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
java.lang.System.set_property("com.drogar.testing.jemmy","true")

require 'manifest'

%w{BasicRobot}.each do |c|
  java_import "org.fest.swing.core."+c
end

%w{GuiActionRunner GuiQuery GuiTask}.each do |c|
  java_import "org.fest.swing.edt."+c
end

%w{JMenuItemFixture FrameFixture JTextComponentFixture}.each do |c|
  java_import "org.fest.swing.fixture."+c
end

%w{JButtonMatcher JLabelMatcher}.each do |c|
  java_import "org.fest.swing.core.matcher."+c
end

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

class AppStarter < GuiQuery
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
  @robot = BasicRobot.robot_with_current_awt_hierarchy
  @qe_frame = FrameFixture.new(@robot, "Quantum Emulator")
end


After do
  title = 'Confirm Exit - PresentationClock'
  @qe_frame.close
  @qe_frame.option_pane.require_title(title).yes_button.click
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