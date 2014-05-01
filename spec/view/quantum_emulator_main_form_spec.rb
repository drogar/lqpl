require 'spec/spec_helper'

describe QuantumEmulatorMainForm do
  before (:each) do

    SwingRunner::on_edt do
      @qemf = QuantumEmulatorMainForm.new
    end
  end
  it "should not be nil" do
    expect(@qemf).not_to be_nil
  end
  specify {expect(@qemf.title).to eq("Quantum Emulator")}
  specify {expect(@qemf.name).to eq("Quantum Emulator")}
  specify {expect(@qemf.content_pane.components.size).to eql(3)}
  context "the messages area" do
    before :each do
      @ma = @qemf.content_pane.components[0]
    end

    it "should be a scroll pane as the first component" do
      expect(@ma.class).to eq(ScrollPane)
    end
    it "should have a text area as the viewport" do
      expect(@ma.viewport.view.class).to eq(JTextArea)
    end
    it "should have a component named 'messagesTextArea' as the viewport" do
      expect(@ma.viewport.view.name).to eq('messagesTextArea')
    end
    it "should have a read-only textarea" do
      expect(@ma.viewport.view.editable).to eq(false)
    end
  end
  context "the spinner area" do
    before :each do
      @spinpanel = @qemf.content_pane.components[1]
    end
    it "should be a panel" do
      expect(@spinpanel.class).to eq(Panel)
    end
    it "should contain 8 children" do
      expect(@spinpanel.components.size).to eql(8)
    end
    it "has spinners for odd components" do
      [1,3,5,7].each do |i|
        expect(@spinpanel.components[i].class).to eq(Spinner)
      end
    end
    it "should have labels for even components" do
      [0,2,4,6].each do |i|
        expect(@spinpanel.components[i].class).to eq(Label)
      end
    end
    it "should have the even component being the label for the spinner" do
      [0,2,4,6].each do |i|
        expect(@spinpanel.components[i].label_for).to eq(@spinpanel.components[i+1])
      end
    end
    specify {expect(@spinpanel.components[0].text).to eq("Step Size")}
    specify {expect(@spinpanel.components[2].text).to eq("Recursion Depth")}
    specify {expect(@spinpanel.components[4].text).to eq("Tree Depth")}
    specify {expect(@spinpanel.components[6].text).to eq("Recursion Multiplier")}
    specify {expect(@spinpanel.components[1].value).to eq(1)}
    specify {expect(@spinpanel.components[3].value).to eq(1)}
    specify {expect(@spinpanel.components[5].value).to eq(4)}
    specify {expect(@spinpanel.components[7].value).to eq(10)}
    specify {expect(@spinpanel.components[1].model.maximum).to eq(100000)}
    specify {expect(@spinpanel.components[3].model.maximum).to eq(100000)}
    specify {expect(@spinpanel.components[5].model.maximum).to eq(100)}
    specify {expect(@spinpanel.components[7].model.maximum).to eq(100000)}
    specify {expect(@spinpanel.components[1].model.minimum).to eq(1)}
    specify {expect(@spinpanel.components[3].model.minimum).to eq(1)}
    specify {expect(@spinpanel.components[5].model.minimum).to eq(1)}
    specify {expect(@spinpanel.components[7].model.minimum).to eq(1)}
    specify {expect(@spinpanel.components[1].model.step_size).to eq(1)}
    specify {expect(@spinpanel.components[3].model.step_size).to eq(1)}
    specify {expect(@spinpanel.components[5].model.step_size).to eq(1)}
    specify {expect(@spinpanel.components[7].model.step_size).to eq(1)}
  end
  context "the button area" do
    before :each do
      @buttonpanel = @qemf.content_pane.components[2]
    end

    it "should be a panel" do
      expect(@buttonpanel.class).to eq(Panel)
    end
    it "should contain 3 children" do
      expect(@buttonpanel.components.size).to eql(3)
    end
    it "should have 3 buttons" do
      @buttonpanel.components.each do |comp|
        expect(comp.class).to eq(Button)
      end
    end
    it "should have 'Trim' as the text of the first button" do
      expect(@buttonpanel.components[0].text).to eq('Trim')
    end
    it "should have 'Step' as the text of the first button" do
      expect(@buttonpanel.components[1].text).to eq('Step')
    end
    it "should have 'Go' as the text of the first button" do
      expect(@buttonpanel.components[2].text).to eq('Go')
    end
    it "should have one row" do
      expect(@buttonpanel.get_layout.rows).to eq(1)
    end
    it "should have three columns" do
      expect(@buttonpanel.get_layout.columns).to eq(3)
    end
  end
  it "should hide the spinner panel" do
    expect(@qemf.content_pane.components[1].visible).to eq(false)
  end
  it "should hide the button panel" do
    expect(@qemf.content_pane.components[2].visible).to eq(false)
  end
  it "should have size of 390x290" do
    expect(@qemf.bounds).to eq(Rectangle.new(10,10,400,300))
  end
  it "should be at 10.10" do
    expect(@qemf.bounds).to eq(Rectangle.new(10,10,400,300))
  end
end