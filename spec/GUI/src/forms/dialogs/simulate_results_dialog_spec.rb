require 'simulate_results_dialog'

describe SimulateResultsDialog do
  before :each do
    SwingRunner.on_edt do
      @sr = SimulateResultsDialog.new
    end
  end

  it 'should not be nil' do
    expect(@sr).not_to be_nil
  end

  it "should have a title of 'Simulate Results'" do
    expect(@sr.edt_title).to eq('Simulate Results')
  end
  specify { expect(@sr.data_pane.components.size).to eql(1) }
  specify { expect(@sr.data_pane.components[0].components.size).to eql(2) }
  # specify {@sr.content_pane.get_layout.class.should == BoxLayout}
  context 'the random value' do
    before :each do
      @rv = @sr.data_pane.components[0].components[0]
    end
    specify { expect(@rv.class).to eq(Label) }
    specify { expect(@sr.random_value_label).to eq(@rv) }
  end

  context 'the results' do
    before :each do
      @res = @sr.data_pane.components[0].components[1]
    end
    specify { expect(@res.class).to eq(ScrollPane) }
    specify { expect(@res.viewport.view.class).to eq(Label) }
    specify { expect(@sr.simulate_results_label).to eq(@res.viewport.view) }
  end
end
