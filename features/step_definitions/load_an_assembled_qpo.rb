When /^I load "([a-zA-Z0-9_\.]*?\.qpo)" from the directory "([^"]*)"$/ do |file, dir|

  java_import org.netbeans.jemmy.operators.JFileChooserOperator
  java_import org.netbeans.jemmy.operators.Operator
  java_import org.netbeans.jemmy.operators.JButtonOperator
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
  p sel_file
  fc.set_selected_file sel_file

  fc.approve_selection


end

Then /^the frame "([^"]*)" should appear$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end

Given /^I have just loaded "([^"]*)" from the directory "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

When /^I click the button "([^"]*)"$/ do |arg1|
  pending # express the regexp above with the code you wish you had
end

Then /^the "([^"]*)" field should show "([^"]*)"$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end
