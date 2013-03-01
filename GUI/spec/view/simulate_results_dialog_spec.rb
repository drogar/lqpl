require 'spec/spec_helper'

describe SimulateResultsDialog do
  before (:each) do
    @sr = SimulateResultsDialog.new
  end

  it "should not be nil" do
    @sr.should_not be_nil
  end
  
  it "should have a title of 'Simulate Results'" do
    @sr.edt_title.should == 'Simulate Results'
  end
  specify {@sr.content_pane.class.should == Panel}
  specify {@sr.content_pane.should have(3).components}
  specify {@sr.content_pane.get_layout.class.should == BoxLayout}
  context "the random value" do
    before (:each) do
      @rv = @sr.content_pane.components[0]
    end
    specify {@rv.class.should == Label}
    specify {@sr.random_value_label.should == @rv}
  end
  
  context "the results" do
    before (:each) do
      @res = @sr.content_pane.components[1]
    end
    specify {@res.class.should == ScrollPane}
    specify {@res.viewport.view.class.should == Label}
    specify {@sr.simulate_results_label.should == @res.viewport.view}
  end
  
  context "the button pane" do
    before (:each) do
      @bp = @sr.content_pane.components[2]
    end
    specify {@bp.class.should == Panel}
    specify {@bp.should have(1).components}
    specify {@bp.components[0].class.should == Button}
    context "the button" do
      before (:each) do
        @btn = @bp.components[0]
      end
      specify {@sr.root_pane.default_button.should == @btn}
      specify {@btn.text.should == "OK"}
      specify {@sr.ok_button.should == @btn}
    end
  end
end