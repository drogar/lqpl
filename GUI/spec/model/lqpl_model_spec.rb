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
  describe "instance methods" do
    before :each do
      @lm = LqplModel.new
    end
    describe "enable_view_menu_items" do
      before :each do
        @lm.enable_view_menu_items
      end
      it "sets view_menu_stack_translation_enabled to true" do
        @lm.view_menu_stack_translation_enabled.should be_true
      end
      it "sets view_menu_dump_enabled to true" do
        @lm.view_menu_dump_enabled.should be_true
      end
      it "sets view_menu_executing_code_enabled to true" do
        @lm.view_menu_executing_code_enabled.should be_true
      end
      it "sets view_menu_classical_stack_enabled to true" do
        @lm.view_menu_classical_stack_enabled.should be_true
      end
    end
    describe "set_title_and_enable" do
      before :each do
        @lm.set_title_and_enable("test")
      end
      it "should set the title to 'Quantum Emulator - <arg>" do
        @lm.frame_title.should == 'Quantum Emulator - test'
      end
      it "should set the go button to enabled" do
        @lm.go_enabled.should be_true
      end
      it "should set the step button to enabled" do
        @lm.step_enabled.should be_true
      end
      it "should set the spinner panel to visible" do
        @lm.spinner_panel_visible.should be_true
      end
      it "should set the button panel to visible" do
        @lm.button_panel_visible.should be_true
      end
      it "should set the message to '<arg> was loaded.'" do
        @lm.messages_text.should == 'test was loaded.'
      end
    end
    describe "toggle_view_menu" do
      it "should switch 'hide' to 'show' and back" do
        @lm.toggle_view_menu(["Hide","Dump"])
        @lm.view_menu_dump_text.should == "Show Dump"
        @lm.toggle_view_menu(["Show","Dump"])
        @lm.view_menu_dump_text.should == "Hide Dump"
      end
    end
  end
end