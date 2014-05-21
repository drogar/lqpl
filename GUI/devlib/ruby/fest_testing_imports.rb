java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
java.lang.System.set_property("com.drogar.testing.fest","true")

%w{GuiActionRunner GuiQuery GuiTask  FailOnThreadViolationRepaintManager}.each do |c|
  java_import "org.fest.swing.edt."+c
end

java_import org.fest.swing.core.BasicRobot
java_import org.fest.swing.finder.WindowFinder

%w{Component JMenuItem Frame JTextComponent JSpinner JLabel JButton JFileChooser}.each do |c|
  java_import "org.fest.swing.fixture."+c+"Fixture"
end

%w{JButton JLabel Frame Dialog}.each do |c|
  java_import "org.fest.swing.core.matcher."+c+"Matcher"
end


#FailOnThreadViolationRepaintManager.install()
