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

  specify {@ab.data_pane.should have(1).components}

  context "the text label" do
    before (:each) do
      @tp = @ab.data_pane.components[0]
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
  
end