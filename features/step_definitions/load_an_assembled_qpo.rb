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
  btn_timeout = Timeouts.new
  btn_timeout.setTimeout("ComponentOperator.WaitComponentTimeout", 100)
  theButton = JButtonOperator.new(@mw, button_text)

  theButton.should_not == nil
  theButton.text.should == button_text
  theButton.should be_visible
end

Then /^the number spinner "([\w\s]*)" with value "([\d]*)" should appear$/ do |spinner_label, spin_value|

  java_import org.netbeans.jemmy.operators.JLabelOperator
  java_import org.netbeans.jemmy.operators.ContainerOperator
  java_import org.netbeans.jemmy.operators.JSpinnerOperator
  theLabel = JLabelOperator.new(@mw, spinner_label)

  theLabel.should_not == nil
  theLabel.text.should == spinner_label
  theLabel.should be_visible

  parent = ContainerOperator.new theLabel.parent
  theSpinner = JSpinnerOperator.new(parent,spin_value)
  theSpiner.should_not == nil
  theSpinner.value.to_string.should == spin_value
end

Then /^the frame "([\w\s]*)" should be visible$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end

Given /^I have just loaded "(.*?)" from the directory "([\w\s\/]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

When /^I click the button "([\w\s]*)"$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end

Then /^the "([\w\s]*)" field should show "(.*?)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end
