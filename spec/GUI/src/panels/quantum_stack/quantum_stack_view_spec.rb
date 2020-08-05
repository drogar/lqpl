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

  describe 'make_quantum_stack_painter' do
    let(:qsp) { instance_double('QuantumStackPainter', image_of_model: 'ICON') }
    it 'Creates a new model' do
      expect(QuantumStackPainter).to receive(:new).with('abc').and_return(qsp)
      @qsv.make_quantum_stack_painter('abc', 'unused')
    end
  end
end
