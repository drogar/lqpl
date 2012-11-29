

Then /^the dialog "([\w\s]*)" should have one of:$/ do |dialog, message_text_table|
  # table is a Cucumber::Ast::Table
  d_op = JDialogOperator.new(dialog)
  message_text = message_text_table.hashes.collect {|h| h.values[0]} # just the list of values
  check_component_contains_label(d_op,message_text).should be_true
  
  theButton = JButtonOperator.new(d_op, "OK")
  theButton.do_click
end