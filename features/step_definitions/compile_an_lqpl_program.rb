


Given /^the frame "([\w\s]*)" is visible$/ do |frame_name|

  eval "@#{frame_name.gsub(/ /,'_')} = FrameFixture.new frame_name"
end

Given /^I select "([a-zA-Z\s]*)" from the "([a-zA-Z]*)" menu$/ do |mitem, menu|

  menu_item =  @qe_frame.menu_item_with_path("menu", "mitem")
  menu_item.should_not be_nil
  menu_item.should be_enabled

  menu_item.click()
  #fmenu_item.push_no_block
end


And /^I load "([\w]*?\.qpl)" from the directory "([\w\s\/]*)"$/ do |file, dir|

  fc = @qe_frame.file_chooser();
  fc.get_dialog_title.should == "Open LQPL File for Compiling"

  fc.get_current_directory.get_absolute_path.should ==  Dir.getwd
  fc.is_file_selection_enabled.should == true
  fc.is_directory_selection_enabled.should == false
  cdir = fc.get_current_directory.get_absolute_path

  if not (cdir =~ /GUI/)
    fc.set_current_directory(java.io.File.new(cdir,"GUI"))
  end
  topdir = fc.get_current_directory.get_absolute_path


  dirs = dir.split("/")
  dirs.each do |d|
    cdir = fc.get_current_directory.get_absolute_path
    fc.set_current_directory (java.io.File.new(cdir, d))
  end
  fc.get_current_directory.get_absolute_path.should ==  topdir+"/"+dir


  sel_file = java.io.File.new(fc.get_current_directory.get_absolute_path,file)

  fc.set_selected_file sel_file

  fc.approve_selection


end


Then /^"([\w\s]*?\.qpo)" should be created in "([\w\s\/]*)" and be equal to "([\w\s\.]*?\.qpo)"$/ do |outfile, outdir,reference|
  topdir = Dir.getwd
  if not (topdir =~ /GUI/)
    topdir = topdir + "/GUI"
  end
  realdir = topdir + "/" +outdir
  theFile = realdir + "/" + outfile
  sleep_until(10) {File.exist?(theFile)}
  # tries = 0
  #   while tries < 10 do # wait up to 2.5 seconds for compile to finish
  #     sleep 0.25
  #     break if File.exist?(theFile)
  #     tries += 1
  #   end
  File.exist?(realdir + "/" + outfile).should be_true
  File.open(realdir + "/" + outfile) do |newone|
    result = newone.read
    File.open(realdir + "/" + reference) do |ref|
      refvalue = ref.read
      result.should == refvalue
    end
  end
end

Then /^the messages field should contain:$/ do |client_message_table|
  theTextArea = JTextAreaOperator.new($qe_frame)
  message_texts = client_message_table.hashes.collect {|h| Regexp.new h.values[0]}
  message_texts.each do |t|
    theTextArea.text.should =~ t
  end

end
