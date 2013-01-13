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
      m = LqplMenu.new(@p)
      m.should_not be_nil
    end
    it "should set the mbar of the parent" do
      LqplMenu.new(@p)
      @p.mbar.should_not be_nil
    end
    it "should set the mbar of the parent to visible" do
      LqplMenu.new(@p)
      @p.mbar.visible.should be_true
    end
    it "should set up two menus (when on mac)" do
      LqplMenu.new(@p)
      @p.mbar.menu_count.should == 2
    end
  end
  TESTMENU_STRUCTURE.each do |m,mtestdata|
    context "#{m} menu" do
      before(:each) do
        @p = Parent.new
        @lm = LqplMenu.new(@p)
        @tmenu = @p.mbar.menu(mtestdata[:menu_index])
      end
      it "should have #{mtestdata[:menu_titles].size} menuitems" do
        @tmenu.item_count.should == mtestdata[:menu_titles].size
      end
      mtestdata[:menu_titles].each_with_index do |mtitle, ind|
        it "should have the title #{mtitle} for item #{ind}" do
          @tmenu.item(ind).text.should == (mtitle)
        end
      end
      mtestdata[:menu_enabled].each_with_index do |is_enabl,ind| 
        it "should have #{is_enabl ? "enabled" : "disabled"} menu item #{ind}" do
          @tmenu.item(ind).enabled.should == is_enabl
        end
      end
    end
  end
end
        
    
      