require 'spec/spec_helper'

require 'spec/specdata/simulate_results_data'
require 'src/dialogs/simulate_results/simulate_results_model'

describe SimulateResultsModel do
  before(:each) do
    @m = SimulateResultsModel.new
  end
  it "should ignore the random value text being set" do
    @m.random_value_text = "junk"
    @m.random_value_text.should == ""
  end
  it "should ignore the simulate results text being set" do
    @m.simulate_results_text = "junk"
    @m.simulate_results_text.should == "<html></html>"
  end
  it "should throw an error if given invalid input" do
    expect {
      @m.simulate_results = "junk"
    }.to raise_error  QuantumStackModelInvalidCreate, /junk/
  end
  it "should set the text or the random value to 'Random Value: rv" do
    @m.simulate_results =ONEELT
    @m.random_value_text.should == "Random Value: 0.27"
  end
  it "should set the simulate results text to an item within html tags" do
    @m.simulate_results =ONEELT
    @m.simulate_results_text.should == "<html>a(Coin) = Heads</html>"
  end
  it "should set the simulate results text to items with <br> tags separating them within html tags" do
    @m.simulate_results =TWOELTS
    @m.simulate_results_text.should == "<html>a(Coin) = Heads<br />q(qubit) = 0</html>"
  end

  describe "class method result_values_to_list" do
    it "should make an empty list with no data" do
      SimulateResultsModel.result_values_to_list("").should == []
    end
    it "should make an empty list with nil data" do
      SimulateResultsModel.result_values_to_list(nil).should == []
    end
    it "should make a singleton list when there is one triple" do
      SimulateResultsModel.result_values_to_list(make_triple("a","Int",37)).should == [["a","Int","37"]]
    end
    it "should make a list of all items" do
      SimulateResultsModel.result_values_to_list(make_triple("a","Int",37)+make_triple("c","Bool","True")+
        make_triple("d","qubit",1)).should == [["a","Int","37"],["c","Bool","True"],["d","qubit","1"]]
    end
  end
end