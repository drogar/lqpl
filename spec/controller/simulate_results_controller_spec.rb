require 'spec/spec_helper'

require 'spec/specdata/simulate_results_data'
require 'GUI/src/dialogs/simulate_results/simulate_results_controller'
require 'GUI/src/dialogs/simulate_results/simulate_results_model'

describe SimulateResultsController do
  before(:each) do
    SwingRunner::on_edt do
      @c = SimulateResultsController.instance
    end
    @st = double("StackTranslation")
    allow(@st).to receive(:reverse_lookup) do |val|
        case val
        when "1" then "@p"
        when "2" then "@q"
        else val
        end
      end

  end
  it "should raise an error when created with junk" do
    expect { @c.set_simulate_data("junk",@st)}. to raise_error   ParserError, /junk/
  end

  it "should create a results set when given the correct input" do
    SwingRunner::on_edt do
      @c.set_simulate_data(TWOELTS,@st)
      expect(@c.simulate_data).to eq("<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>")
    end
  end

end