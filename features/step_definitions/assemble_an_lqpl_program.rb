
Given /^the program "([^"]*)" has started$/ do  |program|
  QUFACE = java_import com.drogar.qface.Main
  #args = [""].to_java(:string)
  begin
    QUFACE.main([])
  rescue
    puts "Had a problem"
  end
end


Given /^the frame "([^"]*)" is visible$/ do |frame_name|

  java_import org.netbeans.jemmy.operators.JFrameOperator
  @mw = JFrameOperator.new frame_name
end

Given /^I select "([a-zA-Z]*)" from the file menu$/ do |mitem|
  java_import org.netbeans.jemmy.operators.JMenuBarOperator

  mbar = JMenuBarOperator.new @mw.get_jmenu_bar
  mbar.push_menu_no_block("File|#{mitem}")
end


And /^I load "([a-zA-Z0-9_]*?\.qpl)" from the directory "([^"]*)"$/ do |file, dir|

  java_import org.netbeans.jemmy.operators.JFileChooserOperator
  java_import org.netbeans.jemmy.operators.Operator
  java_import org.netbeans.jemmy.operators.JButtonOperator
  java_import javax.swing.JButton
  fc = JFileChooserOperator.new
  fc.get_dialog_title.should == "Open LQPL File for Assembling"

  fc.get_current_directory.get_absolute_path.should ==  Dir.getwd
  fc.is_file_selection_enabled.should == true
  fc.is_directory_selection_enabled.should == false

  fl = fc.get_file_list
  puts "is nil fl" if !fl
  exact_string_comp = Operator::DefaultStringComparator.new(true,true)

  #fc.click_on_file (1,2)
  #fc.java_send :enterSubDir, [java.lang.String],"bin"

  dirs = dir.split("/")
  dirs.each do |d|
    cdir = fc.get_current_directory.get_absolute_path
    fc.set_current_directory (java.io.File.new(cdir, d))
  end


  sel_file = java.io.File.new(fc.get_current_directory.get_absolute_path,file)

  fc.set_selected_file sel_file

  fc.approve_selection





  #exact_string_comp = Operator::DefaultStringComparator.new(true,true)
  #puts fc
  #puts fc.get_file_list.get_model.get_size
  #shown = fc.get_files

  #shown.each {|f| puts f.get_name}
  #dirs.each { |d|
  #fc.enter_sub_dir(dirs[0], exact_string_comp)
  #}
  #fc.click_on_file(file, exact_string_comp)
  #fc.approve

end




Then /^"([^"]*)" should be created in "([^"]*)"$/ do |outfile, outdir|
  realdir = Dir.getwd + "/" +outdir
  File.exist?(realdir + outfile).should be_true
end
