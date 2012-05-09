
Given /^the program "(.*?)" has started$/ do  |program|
  QUFACE = java_import com.drogar.qface.Main
  #args = [""].to_java(:string)
  begin
    QUFACE.main([])
  rescue Exception => e
    puts "Exception from main: #{e}"
  end
end


Given /^the frame "([\w\s]*)" is visible$/ do |frame_name|

  java_import org.netbeans.jemmy.operators.JFrameOperator
  @mw = JFrameOperator.new frame_name
end

Given /^I select "([a-zA-Z]*)" from the "([a-zA-Z]*)" menu$/ do |mitem, menu|
  java_import org.netbeans.jemmy.operators.JMenuBarOperator
  java_import org.netbeans.jemmy.operators.JMenuOperator

  mbar = JMenuBarOperator.new @mw.get_jmenu_bar
  jm = mbar.get_menu(0)
  fmenu = JMenuOperator.new jm
  item_count = jm.get_item_count
  ((0...item_count).any? do |i|
    mitem == jm.get_item(i.to_int).get_text
  end).should be_true
  mbar.push_menu_no_block("#{menu}|#{mitem}")
end


And /^I load "([\w]*?\.qpl)" from the directory "([\w\s\/]*)"$/ do |file, dir|

  java_import org.netbeans.jemmy.operators.JFileChooserOperator
  java_import org.netbeans.jemmy.operators.Operator
  java_import org.netbeans.jemmy.operators.JButtonOperator
  java_import javax.swing.JButton
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


  sel_file = java.io.File.new(fc.get_current_directory.get_absolute_path,file)
  p sel_file
  fc.set_selected_file sel_file

  fc.approve_selection


end


Then /^"([\w\s]*)" should be created in "([\w\s\/]*)" and be equal to "([\w\s]*)"$/ do |outfile, outdir,reference|
  realdir = Dir.getwd + "/" +outdir
  File.exist?(realdir + "/" + outfile).should be_true
  File.open(realdir + "/" + outfile) do |newone|
    result = newone.read
    File.open(realdir + "/" + reference) do |ref|
      refvalue = ref.read
      result.should == refvalue
    end
  end
end
