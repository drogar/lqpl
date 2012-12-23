require 'simplecov'
SimpleCov.start

if not (defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby')
  abort 'Sorry - Feature tests of LQPL requires JRuby. You appear to be running or defaulted to some other ruby engine.'
end

where_i_am = File.expand_path(File.dirname(__FILE__))
$project_dir = where_i_am +"/../../"

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
java.lang.System.set_property("com.drogar.testing.fest","true")

require 'manifest'

%w{BasicRobot}.each do |c|
  java_import "org.fest.swing.core."+c
end

%w{GuiActionRunner GuiQuery GuiTask}.each do |c|
  java_import "org.fest.swing.edt."+c
end

%w{Window}.each do |c|
  java_import "org.fest.swing.finder."+c+"Finder"
end

%w{Component JMenuItem Frame JTextComponent JSpinner JLabel JButton JFileChooser}.each do |c|
  java_import "org.fest.swing.fixture."+c+"Fixture"
end

%w{JButton JLabel Frame Dialog}.each do |c|
  java_import "org.fest.swing.core.matcher."+c+"Matcher"
end

java_import java.util.regex.Pattern

java_import java.awt.Component


require 'support/component_query'


class AppStarter < GuiQuery
  # Launch the app in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  # FEST will call this method for us before the test.
  #
  def executeInEDT
    com.drogar.lqpl.Main.main([])
  end
end


# consider starting up servers now and dropping during at_exit.

runner = GuiActionRunner.execute(AppStarter.new)
$robot = BasicRobot.robot_with_current_awt_hierarchy
$qe_frame = FrameFixture.new($robot, "Quantum Emulator")
#$owner = FrameFixture.new($robot,$qe_frame.edt_owner)

at_exit {
  
  #LqplController.instance.close
  $qe_frame.close
  $qe_frame = nil
  $robot.clean_up
  $robot = nil
 
}