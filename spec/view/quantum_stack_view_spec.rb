require 'quantum_stack_view'

describe QuantumStackView do
  before :each do
    SwingRunner.on_edt do
      @qsv = QuantumStackView.new
    end
  end
  it 'should not be nil' do
    expect(@qsv).not_to be_nil
  end
end
