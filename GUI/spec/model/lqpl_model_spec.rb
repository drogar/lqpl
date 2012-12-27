require 'spec/spec_helper'
require 'src/lqpl/lqpl_model'

describe LqplModel do
  describe "toggle_action" do
    it "should return 'Hide' for 'Show'" do
      LqplModel::toggle_action("Show").should == "Hide"
    end
    it "should return 'Show' for 'Hide'" do
      LqplModel::toggle_action("Hide").should == "Show"
    end
    
    it "should return 'Show' for 'anything'" do
      LqplModel::toggle_action("whatever").should == "Show"
    end
  end
  describe "new_view_command" do
    it "should return 'Hide X Y' for input ['Show', 'X', 'Y']" do
      LqplModel::new_view_command(["Show", 'X', 'Y']).should == "Hide X Y"
    end
    it "should return 'Show X Y' for input ['Hide', 'X', 'Y']" do
      LqplModel::new_view_command(["Hide", 'X', 'Y']).should == "Show X Y"
    end
  end
  describe "symbol_for_view_menu_item" do
    it "should return :view_menu_x_text= for input ['whatever','X']" do
      LqplModel::symbol_for_view_menu_item(['whatever','X']).should == :view_menu_x_text=
    end
    it "should return :view_menu_x_y_text= for input ['whatever','X', 'Y']" do
      LqplModel::symbol_for_view_menu_item(['whatever','X', 'Y']).should == :view_menu_x_y_text=
    end
  end
end