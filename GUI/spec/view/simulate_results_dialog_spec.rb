require 'spec/spec_helper'

describe SimulateResultsDialog do
  before (:each) do
    SwingRunner::on_edt do
      @sr = SimulateResultsDialog.new
    end
  end

  it "should not be nil" do
    @sr.should_not be_nil
  end
  
  it "should have a title of 'Simulate Results'" do
    @sr.edt_title.should == 'Simulate Results'
  end
  specify {@sr.data_pane.should have(1).components}
  specify {@sr.data_pane.components[0].should have(2).components}
  #specify {@sr.content_pane.get_layout.class.should == BoxLayout}
  context "the random value" do
    before (:each) do
      @rv = @sr.data_pane.components[0].components[0]
    end
    specify {@rv.class.should == Label}
    specify {@sr.random_value_label.should == @rv}
  end
  
  context "the results" do
    before (:each) do
      @res = @sr.data_pane.components[0].components[1]
    end
    specify {@res.class.should == ScrollPane}
    specify {@res.viewport.view.class.should == Label}
    specify {@sr.simulate_results_label.should == @res.viewport.view}
  end
  
end