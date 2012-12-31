require 'spec/spec_helper'

require 'spec/specdata/simulate_results_data'
require 'src/dialogs/simulate_results/simulate_results_controller'
require 'src/dialogs/simulate_results/simulate_results_model'

describe SimulateResultsController do
  before(:each) do
    @c = SimulateResultsController.instance
    @st = double("StackTranslation")
    @st.stub(:reverse_lookup, :nil? => false) do |val|
        case val
        when "1" then "@p"
        when "2" then "@q"
        else val
        end
      end

  end
  it "should raise an error when created with junk" do
    expect { @c.set_simulate_data("junk",@st)}. to raise_error   ModelCreateError, /junk/
  end

  it "should create a results set when given the correct input" do
    @c.set_simulate_data(TWOELTS,@st)
    @c.get_simulate_data.should == "<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>"
  end

  # it "should set the server_connection when given an sc" do
  #     sc = double('server_connection')
  #     sc.should_receive(:connected?).and_return(true)
  #     @c.lqpl_emulator_server_connection=sc
  #   end
  #   it "should ask the sc for the classical stack when given a depth and recursion" do
  #     sc = double('server_connection')
  #     sc.should_receive(:connected?).and_return(true)
  #     sc.should_receive(:get_simulate_results).and_return(TWOELTS)
  # 
  #     @c.lqpl_emulator_server_connection=sc
  #     @c.set_simulate_results("5",@st)
  #     @c.get_simulate_data.should == "<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>"
  #   end
end