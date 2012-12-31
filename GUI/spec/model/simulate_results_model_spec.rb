require 'spec/spec_helper'

require 'spec/specdata/simulate_results_data'
require 'src/dialogs/simulate_results/simulate_results_model'

describe SimulateResultsModel do
  describe "creation" do
    before(:each) do
      @m = SimulateResultsModel.new
      st = double("StackTranslation", :nil? => false)
      st.stub(:reverse_lookup) do |val|
        case val
        when "1" then "@p"
        when "2" then "@q"
        else val
        end
      end
      @m.stack_translation = st
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
      }.to raise_error  ModelCreateError, /junk/
    end
    it "should set the text or the random value to 'Random Value: rv" do
      @m.simulate_results =ONEELT
      @m.random_value_text.should == "Random Value: 0.27"
    end
    it "should set the simulate results text to an item within html tags" do
      @m.simulate_results =ONEELT
      @m.simulate_results_text.should == "<html>@p(Coin) = Heads</html>"
    end
    it "should set the simulate results text to items with <br> tags separating them within html tags" do
      @m.simulate_results =TWOELTS
      @m.simulate_results_text.should == "<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>"
    end
  end

  describe "class method result_values_to_list" do
    before(:each) do
      @st = double("StackTranslation", :nil? => false)
      @st.stub(:reverse_lookup) do |val|
        case val
        when "1" then "@p"
        when "2" then "@q"
        else val
        end
      end
    end
    it "should make an empty list with no data" do
      SimulateResultsModel.result_values_to_list("",@st).should == []
    end
    it "should make an empty list with nil data" do
      SimulateResultsModel.result_values_to_list(nil,@st).should == []
    end
    it "should make a singleton list when there is one triple" do
      SimulateResultsModel.result_values_to_list(make_triple("1","Int",37),@st).should == [["@p","Int","37"]]
    end
    it "should make a list of all items" do
      SimulateResultsModel.result_values_to_list(make_triple("1","Int",37)+make_triple("2","Bool","True")+
        make_triple("3","qubit",1),@st).should == [["@p","Int","37"],["@q","Bool","True"],["3","qubit","1"]]
    end
  end
end