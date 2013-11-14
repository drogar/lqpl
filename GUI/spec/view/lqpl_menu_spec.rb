require 'spec/spec_helper'

TESTMENU_STRUCTURE = {:file => {:menu_index => 0,
      :menu_titles=>["Load", "Compile", "Simulate"],
      :menu_enabled =>[true,true,true]},
    :view => {:menu_index =>1,
      :menu_titles=>["Hide Classical Stack","Hide Dump","Hide Executing Code","Hide Stack Translation"],
      :menu_enabled =>[false,false,false,false]}}
class Parent 
  attr_accessor :mbar
  def set_menu_bar(mb)
    @mbar = mb
  end
end

describe LqplMenu do
  context "menu initialization" do
    before(:each) do
      @p = Parent.new
    end
    it "should successfully create a new menu" do
      SwingRunner::on_edt do 
        m = LqplMenu.new(@p)
        m.should_not be_nil
      end
    end
    it "should set the mbar of the parent" do
      SwingRunner::on_edt do 
        LqplMenu.new(@p)
        @p.mbar.should_not be_nil
      end
    end
    it "should set the mbar of the parent to visible" do
      SwingRunner::on_edt do 
        LqplMenu.new(@p)
        @p.mbar.visible.should be_true
      end
    end
    it "should set up two menus (when on mac)" do
      java.lang.System.set_property("apple.laf.useScreenMenuBar", "true")
      java.lang.System.set_property("com.drogar.testing.fest","false") # reset so not_on_mac, on_mac work as expected
      SwingRunner::on_edt do 
        LqplMenu.new(@p)
        @p.mbar.menu_count.should == 2
      end
      java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
      java.lang.System.set_property("com.drogar.testing.fest","true")
    end
  end
  TESTMENU_STRUCTURE.each do |m,mtestdata|
    context "#{m} menu" do
      before(:each) do
        java.lang.System.set_property("apple.laf.useScreenMenuBar", "true")
        java.lang.System.set_property("com.drogar.testing.fest","false")
        SwingRunner::on_edt do 
          @p = Parent.new
          @lm = LqplMenu.new(@p)
          @tmenu = @p.mbar.menu(mtestdata[:menu_index])
        end
      end
      after :each do
        java.lang.System.set_property("apple.laf.useScreenMenuBar", "false")
        java.lang.System.set_property("com.drogar.testing.fest","true")
      end
      it "should have #{mtestdata[:menu_titles].size} menuitems" do
        SwingRunner::on_edt {@tmenu.item_count.should == mtestdata[:menu_titles].size}
      end
      mtestdata[:menu_titles].each_with_index do |mtitle, ind|
        it "should have the title #{mtitle} for item #{ind}" do
          SwingRunner::on_edt {@tmenu.item(ind).text.should == (mtitle)}
        end
      end
      mtestdata[:menu_enabled].each_with_index do |is_enabl,ind| 
        it "should have #{is_enabl ? "enabled" : "disabled"} menu item #{ind}" do
          SwingRunner::on_edt {@tmenu.item(ind).enabled.should == is_enabl}
        end
      end
    end
  end
end
        
    
      