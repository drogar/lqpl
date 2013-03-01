require 'spec/spec_helper'

describe AboutDialog do
  before (:each) do
    SwingRunner::on_edt do
      @ab = AboutDialog.new
    end
  end

  it "should not be nil" do
    @ab.should_not be_nil
  end
  
  it "should have a title of 'About LQPL'" do
    @ab.edt_title.should == 'About LQPL'
  end
  specify {@ab.content_pane.class.should == Panel}
  specify {@ab.content_pane.should have(2).components}
  specify {@ab.content_pane.get_layout.class.should == BoxLayout}
  context "the text pane" do
    before (:each) do
      @tp = @ab.content_pane.components[0]
    end
    specify {@tp.class.should == Panel}
    specify {@tp.should have(1).components}
    specify {@tp.components[0].class.should == Label}
    context "the label" do
      before (:each) do
        @lab = @tp.components[0]
      end
      specify {@ab.about_data_label.should == @lab}
    end
  end
  
  context "the button pane" do
    before (:each) do
      @bp = @ab.content_pane.components[1]
    end
    specify {@bp.class.should == Panel}
    specify {@bp.should have(1).components}
    specify {@bp.components[0].class.should == Button}
    context "the button" do
      before (:each) do
        @btn = @bp.components[0]
      end
      specify {@ab.root_pane.default_button.should == @btn}
      specify {@btn.text.should == "OK"}
      specify {@ab.ok_button.should == @btn}
    end
  end
end