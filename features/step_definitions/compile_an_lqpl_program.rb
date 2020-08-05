Given(/^the frame "([\w\s]*)" is visible$/) do |frame_name|
  eval "@#{frame_name.tr(/ /, '_')} = FrameFixture.new frame_name", binding, __FILE__, __LINE__
end

Given(/^I select "([a-zA-Z\s]*)" from the "([a-zA-Z]*)" menu$/) do |mitem, menu|
  click_menu_item([menu, mitem])
  sleep 0.25
end

And(%r{^I load "([\w\.]*?\....)" from the project directory "([\w\s\/]*)"$}) do |file, dir|
  approve_file(dir, file)
end

file_create_matcher = %r{^"([\w\s]*?\.qpo)" should be created in the project directory "([\w\s/]*)" and be equal to "([\w\s\.]*?\.qpo)"$}
Then(file_create_matcher) do |outfile, outdir, reference|
  the_file = File.file_in_project_subdir(outdir, outfile)

  expect(sleep_until_file_exists(10, the_file)).to be true

  expect(File.read(the_file)).to eql(File.read(File.file_in_project_subdir(outdir, reference)))
end

Then(/^the messages field should contain:$/) do |client_message_table|
  all_text_is_in_text_component(client_message_table,
                                JTextComponentFixture.new($robot, 'messagesTextArea'))
end
