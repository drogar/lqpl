


Given /^the frame "([\w\s]*)" is visible$/ do |frame_name|

  eval "@#{frame_name.gsub(/ /,'_')} = FrameFixture.new frame_name"
end

Given /^I select "([a-zA-Z\s]*)" from the "([a-zA-Z]*)" menu$/ do |mitem, menu|

  menu_item =  @qe_frame.menu_item_with_path [menu, mitem].to_java(:string)
  menu_item.should_not be_nil
  menu_item.click()
end


And /^I load "([\w]*?\.qpl)" from the directory "([\w\s\/]*)"$/ do |file, dir|

  fc = @qe_frame.file_chooser();

  cdir =   Dir.getwd

  if not (cdir =~ /GUI/)
    cdir = java.io.File.new(cdir,"GUI")
    fc.set_current_directory(cdir)
  end
  
  dirs = dir.split("/")
  dirs.each do |d|
    cdir = java.io.File.new(cdir, d)
    fc.set_current_directory (cdir)
  end
  
  sel_file = java.io.File.new(cdir,file)

  fc.select_file sel_file

  fc.approve

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
  theTextArea = JTextComponentFixture.new(@robot,"messagesTextArea")
  message_texts = client_message_table.hashes.collect {|h| Regexp.new h.values[0]}
  message_texts.each do |t|
    theTextArea.text.should =~ t
  end

end
