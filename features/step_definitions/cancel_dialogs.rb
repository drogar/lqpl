And(/^I cancel the dialog$/) do
  fc = JFileChooserFixture.new($robot) #   $qe_frame.file_chooser()
  fc.cancel
end
