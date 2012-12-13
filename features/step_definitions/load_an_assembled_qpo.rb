

Then /^the main frame.s title should be "(.*?)"$/ do |the_title|
  ft = GuiActionRunner.execute(FrameTitleQuery.new($qe_frame.component))
  ft.should == the_title
end


Then /^the button "([\w\s]*)" should appear$/ do |button_text|
  theButton = $qe_frame.button(JButtonMatcher.with_text button_text)

  theButton.should_not == nil
  theButton.require_visible
end

Then /^the number spinner "([\w\s]*)" should appear and have value "([\d]*)"$/ do |spinner_label, spin_value|

  theLabel = $qe_frame.label(JLabelMatcher.with_text spinner_label)

  theLabel.should_not == nil
  theLabel.require_visible
  lf = GuiActionRunner.execute(LabelForQuery.new theLabel.component)
  theSpinner = JSpinnerFixture.new($robot,lf)
  theSpinner.should_not == nil
  theSpinner.text.should == "#{spin_value}"
end

Then /^the frame "([\w\s]*)" should (not )?be visible$/ do |frame_name,visible|
   set_frame_name_var(frame_name)
   frame_op = eval(frame_name_var_string frame_name)
   if visible == 'not '
     sleep_until(5) {!frame_op.visible?}
     # tries=0
     #      while tries < 5 do
     #        sleep 0.25
     #        break if !frame_op.visible?
     #      end
     frame_op.should_not be_visible
   else
     sleep_until(5) {frame_op.visible?}
     frame_op.should be_visible
   end
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
  theTextArea = JTextAreaOperator.new(theTabbedPane.selected_component.viewport.view)
  theTextArea.selected_text.chomp.should == selec

end

Then /^the button "([\w\s]*)" on the frame "([\w\s]*)" should be (dis|en)abled$/ do |button_text, frm, dis_or_en|
  the_button = JButtonOperator.new(eval(frame_name_var_string(frm)), button_text)
  the_button.enabled.should == (dis_or_en == 'en')
end
