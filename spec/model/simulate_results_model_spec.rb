# Encoding: UTF-8
require 'spec/spec_helper'

require 'GUI/src/dialogs/simulate_results/simulate_results_model'

ONEELT = '{"Simulated" :0.27, "results": [["1", "Coin", "Heads"]]}'
TWOELTS = '{"Simulated" :0.73, "results": [["1", "Coin", "Heads"], ["2", "qubit", "0"]]}'

describe SimulateResultsModel do
  subject { SimulateResultsModel.new }
  describe 'creation' do
    before(:each) do
      st = double('StackTranslation', :nil? => false)
      allow(st).to receive(:reverse_lookup) do |val|
        case val
        when '1' then '@p'
        when '2' then '@q'
        else val
        end
      end
      subject.stack_translation = st
    end
    it 'should ignore the random value text being set' do
      subject.random_value_text = 'junk'
      expect(subject.random_value_text).to eq('')
    end
    it 'should ignore the simulate results text being set' do
      subject.simulate_results_text = 'junk'
      expect(subject.simulate_results_text).to eq('<html></html>')
    end
    it 'should throw an error if given invalid input' do
      expect { subject.simulate_results = 'junk' }.to raise_error JSON::ParserError, /junk/
    end
    it 'should set the text for the random value to Random Value: rv' do
      subject.simulate_results = ONEELT
      expect(subject.random_value_text).to eq('Random Value: 0.27')
    end
    it 'should set the simulate results text to an item within html tags' do
      subject.simulate_results = ONEELT
      expect(subject.simulate_results_text).to eq('<html>@p(Coin) = Heads</html>')
    end
    it 'should set the simulate results text to items with <br> tags within html tags' do
      subject.simulate_results = TWOELTS
      expect(subject.simulate_results_text)
        .to eq('<html>@p(Coin) = Heads<br />@q(qubit) = 0</html>')
    end
  end
end
