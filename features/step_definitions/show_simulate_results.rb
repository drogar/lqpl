
Then /^the dialog "([\w\s]*)" should have one of:$/ do |dialog, message_text_table|
  # table is a Cucumber::Ast::Table
  d_op = JDialogOperator.new(dialog)
  message_text = message_text_table.hashes.collect {|h| h.values[0]} # just the list of values
  found = false
  d_op.components.each do |comp|
    case comp
    when com.javax.swing.JLabel
      found |= message_text.any? {|mt| comp.text =~ mt}
    end
  end
  found.should be_true
end