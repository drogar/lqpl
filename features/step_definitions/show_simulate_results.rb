# encoding: UTF-8
Then(/^the dialog "([\w\s]*)" should have one of:$/) do |dialog_title, message_text_table|
  # table is a Cucumber::Ast::Table
  dialog_fixture = WindowFinder.find_dialog(DialogMatcher
                                            .with_title(dialog_title)).using($robot)
  the_display_area_label = dialog_fixture.label(label_matcher_with_pattern('<html>.*'))

  expect(any_text_is_in_text_component(message_text_table, the_display_area_label)).to be true

  dialog_fixture.button.click
end

Then(/^the dialog "([\w\s]*)" should have one of the following in its only label:$/) do |dialog_title, message_text_table|
  # table is a Cucumber::Ast::Table
  dialog_fixture = WindowFinder.find_dialog(DialogMatcher
                                            .with_title(dialog_title)).using($robot)
  the_display_area_label = dialog_fixture.label
  expect(any_text_is_in_text_component(message_text_table, the_display_area_label)).to be true
  dialog_fixture.button.click
end
