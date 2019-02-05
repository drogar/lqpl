Given(/^I type "(\d+)" in the "(.*?)" field$/) do |val, field_name|
  spinner = spinner_for_label field_name
  spinner.enter_text_and_commit val.to_s
end

Given(/^I click the (up|down) on the "(.*?)" spinner (\d+) times?$/) do |up_down, field_name, count|
  change_spinner_for_label(field_name, count.to_i, up_down)
end

Then(/^I click the spinner "([\w\s]*)" (up|down) (\d+) times? on the frame "([\w\s]*)"$/) do |spinner_label, direction, count, frame_title|
  frame_ref = set_and_return_frame_fixture(frame_title)
  change_spinner_for_label(spinner_label, count.to_i, direction, frame_ref)
end

Then(/^the number spinner "([\w\s]*)" should appear and have value "(\d+)"$/) do |spinner_label, spin_value|
  spinner = spinner_for_label_should_be_visible(spinner_label)
  expect(spinner.text).to eql(spin_value.to_s)
end

Then(/^the number spinner "([\w\s]*)" should appear$/) do |spinner_label|
  spinner_for_label_should_be_visible(spinner_label)
end
