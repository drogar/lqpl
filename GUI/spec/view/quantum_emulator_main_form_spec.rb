require 'spec/spec_helper'

describe QuantumEmulatorMainForm do
  before (:each) do
    
    #SwingRunner::on_edt do
      @qemf = QuantumEmulatorMainForm.new
    #end
  end
  it "should not be nil" do
    @qemf.should_not be_nil
  end
  specify {@qemf.title.should == "Quantum Emulator"}
  specify {@qemf.name.should == "Quantum Emulator"}
  specify {@qemf.content_pane.should have(3).components}
  context "the messages area" do
    before :each do
      @ma = @qemf.content_pane.components[0]
    end
    
    it "should be a scroll pane as the first component" do
      @ma.class.should == ScrollPane
    end
    it "should have a text area as the viewport" do
      @ma.viewport.view.class.should == JTextArea
    end
    it "should have a component named 'messagesTextArea' as the viewport" do
      @ma.viewport.view.name.should == 'messagesTextArea'
    end
    it "should have a read-only textarea" do
      @ma.viewport.view.editable.should == false
    end
  end
  context "the spinner area" do
    before :each do
      @spinpanel = @qemf.content_pane.components[1]
    end
    it "should be a panel" do
      @spinpanel.class.should == Panel
    end
    it "should contain 8 children" do
      @spinpanel.should have(8).components
    end
    it "has spinners for odd components" do
      [1,3,5,7].each do |i|
        @spinpanel.components[i].class.should == Spinner
      end
    end
    it "should have labels for even components" do
      [0,2,4,6].each do |i|
        @spinpanel.components[i].class.should == Label
      end
    end
    it "should have the even component being the label for the spinner" do
      [0,2,4,6].each do |i|
        @spinpanel.components[i].label_for.should == @spinpanel.components[i+1]
      end
    end
    specify {@spinpanel.components[0].text.should == "Step Size"}
    specify {@spinpanel.components[2].text.should == "Recursion Depth"}
    specify {@spinpanel.components[4].text.should == "Tree Depth"}
    specify {@spinpanel.components[6].text.should == "Recursion Multiplier"}
    specify {@spinpanel.components[1].value.should == 1}
    specify {@spinpanel.components[3].value.should == 1}
    specify {@spinpanel.components[5].value.should == 4}
    specify {@spinpanel.components[7].value.should == 10}
    specify {@spinpanel.components[1].model.maximum.should == 100000}
    specify {@spinpanel.components[3].model.maximum.should == 100000}
    specify {@spinpanel.components[5].model.maximum.should == 100}
    specify {@spinpanel.components[7].model.maximum.should == 100000}
    specify {@spinpanel.components[1].model.minimum.should == 1}
    specify {@spinpanel.components[3].model.minimum.should == 1}
    specify {@spinpanel.components[5].model.minimum.should == 1}
    specify {@spinpanel.components[7].model.minimum.should == 1}
    specify {@spinpanel.components[1].model.step_size.should == 1}
    specify {@spinpanel.components[3].model.step_size.should == 1}
    specify {@spinpanel.components[5].model.step_size.should == 1}
    specify {@spinpanel.components[7].model.step_size.should == 1}
  end
  context "the button area" do
    before :each do
      @buttonpanel = @qemf.content_pane.components[2]
    end
    
    it "should be a panel" do
      @buttonpanel.class.should == Panel
    end
    it "should contain 3 children" do
      @buttonpanel.should have(3).components
    end
    it "should have 3 buttons" do
      @buttonpanel.components.each do |comp|
        comp.class.should == Button
      end
    end
    it "should have 'Trim' as the text of the first button" do
      @buttonpanel.components[0].text.should == 'Trim'
    end
    it "should have 'Step' as the text of the first button" do
      @buttonpanel.components[1].text.should == 'Step'
    end
    it "should have 'Go' as the text of the first button" do
      @buttonpanel.components[2].text.should == 'Go'
    end
    it "should have one row" do
      @buttonpanel.get_layout.rows.should == 1
    end
    it "should have three columns" do
      @buttonpanel.get_layout.columns.should == 3
    end
  end
  it "should hide the spinner panel" do
    @qemf.content_pane.components[1].visible.should == false
  end
  it "should hide the button panel" do
    @qemf.content_pane.components[2].visible.should == false
  end
  it "should have size of 390x290" do
    @qemf.bounds.should == Rectangle.new(10,10,400,300)
  end
  it "should be at 10.10" do
    @qemf.bounds.should == Rectangle.new(10,10,400,300)
  end
end