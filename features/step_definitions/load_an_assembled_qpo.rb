

Then /^the main frame.s title should be "(.*?)"$/ do |the_title|
  ft = GuiActionRunner.execute(TitleQuery.new($qe_frame))
  ft.should == the_title
end


Then /^the button "([\w\s]*)" should appear$/ do |button_text|
  theButton = $qe_frame.button(JButtonMatcher.with_text button_text)

  theButton.should_not == nil
  theButton.should be_edt_visible
end

Then /^the number spinner "([\w\s]*)" should appear and have value "([\d]*)"$/ do |spinner_label, spin_value|
  theSpinner = spinner_for_label(spinner_label)
  theSpinner.should_not == nil
  theSpinner.should be_edt_visible
  theSpinner.text.should == "#{spin_value}"
end

Then /^the frame "([\w\s]*)" should (be|not be) visible$/ do |frame_title,visible|
  frame_fixture = set_and_return_frame_fixture(frame_title)
  sleep_until_visibility(5,frame_fixture,visible).should be_true
end

Then /^I click the spinner "([\w\s]*)" (up|down) (\d)* times? on the frame "([\w\s]*)"$/ do |spinner_label, direction, count, frame_title|
  frame_ref=set_and_return_frame_fixture(frame_title)
  theSpinner = spinner_for_label(spinner_label,frame_ref)
  theSpinner.increment(count.to_i) if direction == "up"
  theSpinner.decrement(count.to_i) if direction == "down"
  
end

When /^I click the button "([\w\s]*)" (\d)* times? on the frame "([\w\s]*)"$/ do |button_text, count, frame_title|
  frame_ref=set_and_return_frame_fixture(frame_title)
  theButton = frame_ref.button(JButtonMatcher.with_text button_text)
  count.to_i.times {|i| theButton.click}

end

Then /^the selection on the frame "([\w\s]*)" should show ---(.*?)$/ do |frame_title, selection|
  frame_ref=set_and_return_frame_fixture(frame_title)
  
  theTabbedPane = frame_ref.tabbed_pane
  theTextArea = theTabbedPane.edt_selected_component.edt_viewport.edt_view
  theTextArea.selected_text.chomp.should == selection

end

Then /^the button "([\w\s]*)" on the frame "([\w\s]*)" should be (dis|en)abled$/ do |button_text, frame_title, dis_or_en|
  frame_ref=set_and_return_frame_fixture(frame_title)
  the_button = frame_ref.button(JButtonMatcher.with_text button_text)
  the_button.edt_enabled?.should == (dis_or_en == 'en')
end
