

Then /^the main frame.s title should be "(.*?)"$/ do |the_title|
  ft = GuiActionRunner.execute(TitleQuery.new($qe_frame))
  expect(ft).to eql(the_title)
end


Then /^the button "([\w\s]*)" should appear$/ do |button_text|
  theButton = $qe_frame.button(JButtonMatcher.with_text button_text)

  expect(theButton).not_to be_nil
  expect(theButton).to be_edt_visible
end



Then /^the frame "([\w\s]*)" should (be|not be) visible$/ do |frame_title,visible|
  frame_fixture = set_and_return_frame_fixture(frame_title)
  expect(sleep_until_visibility(5,frame_fixture,visible)).to be true
end


When /^I click the button "([\w\s]*)" (\d+) times? on the frame "([\w\s]*)"$/ do |button_text, count, frame_title|
  frame_ref=set_and_return_frame_fixture(frame_title)
  theButton = frame_ref.button(JButtonMatcher.with_text button_text)
  count.times {|i| theButton.click}

end

Then /^the selection on the frame "([\w\s]*)" should show ---(.*?)$/ do |frame_title, selection|
  frame_ref=set_and_return_frame_fixture(frame_title)

  theTabbedPane = frame_ref.tabbed_pane
  theTextArea = theTabbedPane.edt_selected_component.edt_viewport.edt_view
  expect(theTextArea.selected_text.chomp).to eql(selection)

end

Then /^the button "([\w\s]*)" on the frame "([\w\s]*)" should be (dis|en)abled$/ do |button_text, frame_title, dis_or_en|
  frame_ref=set_and_return_frame_fixture(frame_title)
  the_button = frame_ref.button(JButtonMatcher.with_text button_text)
  expect(the_button.edt_enabled?).to eql(dis_or_en == 'en')
end
