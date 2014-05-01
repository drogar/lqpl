require 'spec/spec_helper'

require 'spec/specdata/simulate_results_data'
require 'GUI/src/dialogs/simulate_results/simulate_results_model'

describe SimulateResultsModel do
  describe "creation" do
    before(:each) do
      @m = SimulateResultsModel.new
      st = double("StackTranslation", :nil? => false)
      allow(st).to receive(:reverse_lookup) do |val|
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
      expect(@m.random_value_text).to eq("")
    end
    it "should ignore the simulate results text being set" do
      @m.simulate_results_text = "junk"
      expect(@m.simulate_results_text).to eq("<html></html>")
    end
    it "should throw an error if given invalid input" do
      expect {
        @m.simulate_results = "junk"
      }.to raise_error  ParserError, /junk/
    end
    it "should set the text for the random value to 'Random Value: rv" do
      @m.simulate_results =ONEELT
      expect(@m.random_value_text).to eq("Random Value: 0.27")
    end
    it "should set the simulate results text to an item within html tags" do
      @m.simulate_results =ONEELT
      expect(@m.simulate_results_text).to eq("<html>@p(Coin) = Heads</html>")
    end
    it "should set the simulate results text to items with <br> tags separating them within html tags" do
      @m.simulate_results =TWOELTS
      expect(@m.simulate_results_text).to eq("<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>")
    end
  end

 
end