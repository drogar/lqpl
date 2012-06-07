

When /I load "([a-zA-Z0-9_\.]*?\.qpo)" from the directory "([\w\s\/]*)"/ do |file, dir|

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
  theButton = JButtonOperator.new($qe_frame, button_text)

  theButton.should_not == nil
  theButton.text.should == button_text
  theButton.should be_visible
end

Then /^the number spinner "([\w\s]*)" should appear and have value "([\d]*)"$/ do |spinner_label, spin_value|

  theLabel = JLabelOperator.new($qe_frame, spinner_label)

  theLabel.should_not == nil
  theLabel.text.should == spinner_label
  theLabel.should be_visible

  theSpinner = JSpinnerOperator.new(theLabel.label_for)
  theSpinner.should_not == nil
  "#{theSpinner.value}".should == spin_value
end

Then /^the frame "([\w\s]*)" should be visible$/ do |frame_name|
   set_frame_name_var(frame_name)
end

Then /^I click the spinner "([\w\s]*)" (up|down) (\d)* times? on the frame "([\w\s]*)"$/ do |spinner_label, direction, count, frm|
  theSpinner = JSpinnerOperator.new(JLabelOperator.new(eval(frame_name_var_string frm), spinner_label).label_for)
  spin_button = theSpinner.increase_operator
  spin_button = theSpinner.decrease_operator if direction == "down"
  count.to_i.times {|i| spin_button.do_click}

end

When /^I click the button "([\w\s]*)" (\d)* times? on the frame "([\w\s]*)"$/ do |button_text, count, frm|
  theButton = JButtonOperator.new(eval(frame_name_var_string(frm)), button_text)
  count.to_i.times {|i| theButton.do_click}

end

Then /^the selection on the frame "([\w\s]*)" should show ---(.*?)$/ do |frame_name, selec|

  theTabbedPane = JTabbedPaneOperator.new(eval(frame_name_var_string(frame_name)))
  theTextArea = JTextAreaOperator.new(theTabbedPane.selected_component)
  theTextArea.selected_text.chomp.should == selec

end

Then /^the button "([\w\s]*)" on the frame "([\w\s]*)" should be (dis|en)abled$/ do |button_text, frm, dis_or_en|
  the_button = JButtonOperator.new(eval(frame_name_var_string(frm)), button_text)
  the_button.enabled.should == (dis_or_en == 'en')
end
