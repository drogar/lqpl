


Given /^the frame "([\w\s]*)" is visible$/ do |frame_name|

  eval "@#{frame_name.gsub(/ /,'_')} = JFrameOperator.new frame_name"
end

Given /^I select "([a-zA-Z\s]*)" from the "([a-zA-Z]*)" menu$/ do |mitem, menu|

  mbar = JMenuBarOperator.new $qe_frame.get_jmenu_bar
  #jm = mbar.get_menu(0)
  fmenu = JMenuOperator.new(mbar,menu)
  fmenu.should_not be_nil
  item_count = fmenu.get_item_count
  ((0...item_count).any? do |i|
    mitem == fmenu.get_item(i.to_int).get_text
  end).should be_true
  jmi = nil
  (0...item_count).each do |i|
    jmi = fmenu.get_item(i.to_int) if mitem == fmenu.get_item(i.to_int).get_text
  end
  fmenu_item = JMenuItemOperator.new(jmi)
  fmenu_item.should_not be_nil
  fmenu_item.should be_enabled

  mbar.push_menu_no_block("#{menu}|#{mitem}")
  #fmenu_item.push_no_block
end


And /^I load "([\w]*?\.qpl)" from the directory "([\w\s\/]*)"$/ do |file, dir|

  fc = JFileChooserOperator.new
  fc.get_dialog_title.should == "Open LQPL File for Compiling"

  fc.get_current_directory.get_absolute_path.should ==  Dir.getwd
  fc.is_file_selection_enabled.should == true
  fc.is_directory_selection_enabled.should == false

  fl = fc.get_file_list
  exact_string_comp = Operator::DefaultStringComparator.new(true,true)


  dirs = dir.split("/")
  dirs.each do |d|
    cdir = fc.get_current_directory.get_absolute_path
    fc.set_current_directory (java.io.File.new(cdir, d))
  end
  fc.get_current_directory.get_absolute_path.should ==  Dir.getwd+"/"+dir


  sel_file = java.io.File.new(fc.get_current_directory.get_absolute_path,file)

  fc.set_selected_file sel_file

  fc.approve_selection


end


Then /^"([\w\s]*?\.qpo)" should be created in "([\w\s\/]*)" and be equal to "([\w\s\.]*?\.qpo)"$/ do |outfile, outdir,reference|
  realdir = Dir.getwd + "/" +outdir
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
