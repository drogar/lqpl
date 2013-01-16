require 'simplecov'
SimpleCov.start do
  add_filter "features"
  nocov_token = ":nocov:"
end

if not (defined? RUBY_ENGINE && RUBY_ENGINE == 'jruby')
  abort 'Sorry - Feature tests of LQPL requires JRuby. You appear to be running or defaulted to some other ruby engine.'
end

project_dir_array = File.expand_path(File.dirname(__FILE__)).split(File::SEPARATOR)

project_dir = project_dir_array.reverse.drop(2).reverse.join(File::SEPARATOR)


%w{src lqpl_gui lib/java lib/ruby devlib/java}.each do |dir|
  $LOAD_PATH << project_dir+"/GUI/"+ dir
end
$LOAD_PATH << project_dir+"/out/lqpl_gui"


require 'java'

$CLASSPATH << project_dir+"/out/lqpl_gui"

#runtime jars
%w{jruby-complete forms_rt monkeybars-1.1.1}.each do |jar|
  $CLASSPATH << project_dir+"/GUI/lib/java/"+jar+".jar"
end

#testing jars
%w{fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2 fest-util-1.1.2 jcip-annotations-1.0}.each do |jar|
  $CLASSPATH << project_dir+"/GUI/devlib/java/" + jar+".jar"
end


require "fest-swing-1.2.jar"

%w{GuiActionRunner GuiQuery GuiTask}.each do |c|
  java_import "org.fest.swing.edt."+c
end

#
# Thought I'd try in the EDT thread - but nope - still doesn't get to it.
# even tried combining with AppRunner below, still doesn't
#
# class ScovStarter < GuiQuery
#   # Launch the app in the Event Dispatch Thread (EDT),
#   # which is the thread reserved for user interfaces.
#   # FEST will call this method for us before the test.
#   #
#   def executeInEDT
#     SimpleCov.start do
#       add_filter "features"
#     end
#   end
# end
# 
# GuiActionRunner.execute(ScovStarter.new)

require "monkeybars-1.1.1.jar"
require "forms_rt.jar"

ENV['PATH'] = "#{project_dir + '/out/bin'}#{File::PATH_SEPARATOR}#{ENV['PATH']}"

java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
java.lang.System.set_property("com.drogar.testing.fest","true")

require 'manifest'

%w{BasicRobot}.each do |c|
  java_import "org.fest.swing.core."+c
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
    LqplController.instance.open #com.drogar.lqpl.Main.main([])
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