

Then /^the dialog "([\w\s]*)" should have one of:$/ do |dialog_title, message_text_table|
  # table is a Cucumber::Ast::Table
  dialog_fixture = WindowFinder.find_dialog(DialogMatcher.with_title (dialog_title)).using($robot)
  the_display_area_label = dialog_fixture.label(JLabelMatcher.with_text(Pattern.compile("<html>.*html>")))
  message_text = message_text_table.hashes.collect {|h| h.values[0]} # just the list of values
  
  message_texts = message_text_table.hashes.collect {|h| Regexp.new h.values[0]}
  message_shown = message_texts.any? do |t|
    the_display_area_label.text =~ t
  end
  message_shown.should == true
  
  the_button = dialog_fixture.button()
  the_button.click
end