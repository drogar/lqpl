


Given /^the frame "([\w\s]*)" is visible$/ do |frame_name|

  eval "@#{frame_name.gsub(/ /,'_')} = FrameFixture.new frame_name"
end

Given /^I select "([a-zA-Z\s]*)" from the "([a-zA-Z]*)" menu$/ do |mitem, menu|

  menu_item =  $qe_frame.menu_item_with_path [menu, mitem].to_java(:string)
  menu_item.should_not be_nil
  menu_item.click()
  sleep 0.25
end


And /^I load "([\w\.]*?\....)" from the project directory "([\w\s\/]*)"$/ do |file, dir|

  fc = JFileChooserFixture.new($robot) #   $qe_frame.file_chooser()

  fc.select_file_in_project_directory(dir,file)

  fc.approve

end


Then /^"([\w\s]*?\.qpo)" should be created in the project directory "([\w\s\/]*)" and be equal to "([\w\s\.]*?\.qpo)"$/ do |outfile, outdir,reference|
  the_file = File.file_in_project_subdir(outdir, outfile)
  
  sleep_until_file_exists(10,the_file).should be_true
  
  File.read(the_file).should == File.read(File.file_in_project_subdir(outdir,reference))
end

Then /^the messages field should contain:$/ do |client_message_table|
  all_text_is_in_text_component(client_message_table, JTextComponentFixture.new($robot,"messagesTextArea"))
end
