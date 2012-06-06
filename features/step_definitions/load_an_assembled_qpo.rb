

When /I load "([a-zA-Z0-9_\.]*?\.qpo)" from the directory "([\w\s\/]*)"/ do |file, dir|

  java_import org.netbeans.jemmy.operators.JFileChooserOperator
  java_import org.netbeans.jemmy.operators.Operator
  java_import javax.swing.JButton
  fc = JFileChooserOperator.new
  fc.get_dialog_title.should == "Load LQPO (Assembly) File"

  fc.get_current_directory.get_absolute_path.should ==  Dir.getwd
  fc.is_file_selection_enabled.should == true
  fc.is_directory_selection_enabled.should == false

  fl = fc.get_file_list
  exact_string_comp = Operator::DefaultStringComparator.new(true,true)


  dirs = dir.split("/")
  dirs.each do |d|
    cdir = fc.get_current_directory.get_absolute_path
    fc.set_current_directory (java.io.File.new(cdir, d))
  end


  sel_file = java.io.File.new(fc.get_current_directory.get_absolute_path,file)
#  p sel_file
  fc.set_selected_file sel_file

  fc.approve_selection


end

Then /^the button "([\w\s]*)" should appear$/ do |button_text|
  java_import org.netbeans.jemmy.operators.JButtonOperator
  java_import org.netbeans.jemmy.Timeouts
#  btn_timeout = Timeouts.new
#  btn_timeout.setTimeout("ComponentOperator.WaitComponentTimeout", 100)
  theButton = JButtonOperator.new($qe_frame, button_text)

  theButton.should_not == nil
  theButton.text.should == button_text
  theButton.should be_visible
end

Then /^the number spinner "([\w\s]*)" with value "([\d]*)" should appear$/ do |spinner_label, spin_value|

  java_import org.netbeans.jemmy.operators.JLabelOperator
  java_import org.netbeans.jemmy.operators.ContainerOperator
  java_import org.netbeans.jemmy.operators.JSpinnerOperator
  theLabel = JLabelOperator.new($qe_frame, spinner_label)

  theLabel.should_not == nil
  theLabel.text.should == spinner_label
  theLabel.should be_visible

  parent = ContainerOperator.new theLabel.parent
  theSpinner = JSpinnerOperator.new(parent,spin_value)
  theSpinner.should_not == nil
  "#{theSpinner.value}".should == spin_value
end

Then /^the frame "([\w\s]*)" should be visible$/ do |frame_name|
   java_import org.netbeans.jemmy.operators.JFrameOperator
   set_frame_name_var(frame_name)
end


When /^I click the button "([\w\s]*)" (\d)* times? on the frame "([\w\s]*)"$/ do |button_text, count, frm|
  java_import org.netbeans.jemmy.operators.JButtonOperator
  java_import org.netbeans.jemmy.Timeouts

  theButton = JButtonOperator.new(eval(frame_name_var_string(frm)), button_text)
  count.to_i.times {|i| theButton.do_click}

end

Then /^the selection on the frame "([\w\s]*)" should show ---(.*?)$/ do |frame_name, selec|
  java_import org.netbeans.jemmy.operators.JTabbedPaneOperator
  java_import org.netbeans.jemmy.operators.JTextAreaOperator

  theTabbedPane = JTabbedPaneOperator.new(eval(frame_name_var_string(frame_name)))
  theTextArea = JTextAreaOperator.new(theTabbedPane.selected_component)
  selec.should == theTextArea.selected_text.chomp

end
