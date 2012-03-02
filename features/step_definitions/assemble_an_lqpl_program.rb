
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
  fc = JFileChooserOperator.new
  fc.get_dialog_title.should == "Open LQPL File for Assembling"

end




Then /^"([^"]*)" should be created in "([^"]*)"$/ do |outfile, outdir|
  pending # express the regexp above with the code you wish you had
end
