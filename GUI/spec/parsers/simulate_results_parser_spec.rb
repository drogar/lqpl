require 'spec/spec_helper'
require 'spec/specdata/simulate_results_data'

describe SimulateResultsParser do
  it "should set the text for the random value to 'Random Value: rv" do
    srp = SimulateResultsParser.new ONEELT
    srp.random_value.should == "0.27"
  end
  it "should set the simulate results to a triple of the stackaddress, type and value for single element" do
    srp = SimulateResultsParser.new ONEELT
    srp.simulate_results.should == [["1","Coin","Heads"]]
  end
  it "should set the simulate results to two triples of the stackaddress, type and value for two elements" do
    srp = SimulateResultsParser.new TWOELTS
    srp.simulate_results.should == [["1","Coin","Heads"],["2","qubit","0"]]
  end
end