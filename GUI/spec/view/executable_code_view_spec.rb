require 'spec/spec_helper'

require 'src/panels/executable_code/executable_code_view'
require 'src/panels/executable_code/code_pointer'

QPOLINES=["1   someline","2 another line"]
CP = CodePointer.new("<pair><string>m</string><int>1</int></pair>")
QPOLINES2 = ["1 entry 1", "2 entry 2", "3 entry 3"]

CM = {:m => QPOLINES, :s => QPOLINES2}

describe ExecutableCodeView do
  describe "init_instructions_text_area" do
    before :each do
      SwingRunner::on_edt do 
        @ta = ExecutableCodeView::init_instructions_text_area QPOLINES
      end
    end
    it "should create a text area with the text passed in" do
      SwingRunner::on_edt {@ta.text.should =~ /someline/}
    end
    it "should not be an editable text area" do
      SwingRunner::on_edt {@ta.editable.should be_false}
    end
    it "should have selection start and end of 0" do
      SwingRunner::on_edt do
        @ta.selection_start.should == 0
        @ta.selection_end.should == 0
      end
    end
  end
  describe "init_scroll_pane" do
    it "should return a JScrollPane" do
      SwingRunner::on_edt do
        jsp = ExecutableCodeView::init_scroll_pane QPOLINES
        jsp.class.should == JScrollPane
      end
    end
    it "should contain a text area with the text passed in." do      
      SwingRunner::on_edt do
        jsp = ExecutableCodeView::init_scroll_pane QPOLINES
        jsp.viewport.view.text.should =~ /someline/
      end
    end
  end
  describe "reset_tabbed_panes_and_maps" do
    before :each do
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
      end
    end
    it "should reset the code_tabbed_pane to have no tabs" do
      @ecv.reset_tabbed_panes_and_maps
      @ecv.code_tabbed_pane.tab_count.should == 0
    end
    it "should reset the map method_to_tab to be empty" do
      @ecv.qpo_method_to_tab_map = {:a => "b"}
      @ecv.reset_tabbed_panes_and_maps
      @ecv.qpo_method_to_tab_map.should == {}
    end
  end
  describe "add_to_selection_start_and_end_map" do
    before :each do
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
        @ecv.qpo_method_and_line_to_selection_start_and_end_map = {}
      end
    end
    it "adds 'm--1 =>[5,10]' for input cp('m',1),'1234',5" do
      @ecv.add_to_selection_start_and_end_map(CP,'1234',5)
      @ecv.qpo_method_and_line_to_selection_start_and_end_map["m--1"].should == [5,10]
    end
  end
  describe "set_selection_bounds_in_view" do
    before :each do
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
        @ecv.reset_tabbed_panes_and_maps
        @ecv.code_tabbed_pane.add_tab("m",ExecutableCodeView::init_scroll_pane(QPOLINES))
        @ecv.code_tabbed_pane.selected_index = 0
      end
    end
    it "should set the selection bounds to 2,5 on input of [2,5]" do
      SwingRunner::on_edt do
        @ecv.set_selection_bounds_in_view [2,5]
        jt = @ecv.code_tabbed_pane.selected_component.viewport.view
        jt.selection_start.should == 2
        jt.selection_end.should == 5
      end
    end
  end
  describe "set_highlight_for_code_pointer" do
    before :each do
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
        @ecv.reset_tabbed_panes_and_maps
        @ecv.code_tabbed_pane.add_tab("m",ExecutableCodeView::init_scroll_pane(QPOLINES))
        @ecv.add_to_selection_start_and_end_map(CP,'1234',5)
        @ecv.qpo_method_to_tab_map[CP.qpo_method] = 0
      end
    end
    it "should set the bounds to 5,10 for input CP" do
      SwingRunner::on_edt do
        @ecv.set_highlight_for_code_pointer(CP)
        jt = @ecv.code_tabbed_pane.selected_component.viewport.view
        jt.selection_start.should == 5
        jt.selection_end.should == 10
      end
    end
  end
  describe "create_tabbed_view" do
    before :each do
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
        @ecv.create_tabbed_views(CM)
      end
    end
    it "should have two panes" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.tab_count.should == 2
      end
    end
    it "should have /someline/ in the first tab" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.selected_index = 0
        @ecv.code_tabbed_pane.selected_component.viewport.view.text.should =~ /someline/
      end
    end
    it "should have /entry/ in the second tab" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.selected_index = 1
        @ecv.code_tabbed_pane.selected_component.viewport.view.text.should =~ /entry/
      end
    end
  end
  describe "set_up_tabbed_views" do
    before :each do
      md = double("ApplicationModel", :nil? => false)
      md.stub(:the_code).and_return(CM)
      md.stub(:the_code_pointer).and_return(CP)
      md.stub(:the_code_was_updated?).and_return(true)
      SwingRunner::on_edt do
        @ecv = ExecutableCodeView.new
        @ecv.set_up_tabbed_views(md,nil)
      end
    end
    it "should have two panes" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.tab_count.should == 2
      end
    end
    it "should have /someline/ in the first tab" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.selected_index = 0
        @ecv.code_tabbed_pane.selected_component.viewport.view.text.should =~ /someline/
      end
    end
    it "should have /entry/ in the second tab" do
      SwingRunner::on_edt do
        @ecv.code_tabbed_pane.selected_index = 1
        @ecv.code_tabbed_pane.selected_component.viewport.view.text.should =~ /entry/
      end
    end
  end
end