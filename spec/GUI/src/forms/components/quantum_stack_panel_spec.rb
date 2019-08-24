require 'quantum_stack_panel'
describe QuantumStackPanel do
  before :each do
    SwingRunner.on_edt do
      @subject = QuantumStackPanel.new
    end
  end

  it 'should not be nil' do
    expect(@subject).not_to be_nil
  end

  describe 'quantum_stack_painter=' do
    let(:qsp) { instance_double('QuantumStackPainter', image_of_model: 'ICON') }
    it 'sets the quantum stack painter' do
      @subject.quantum_stack_painter = qsp
      expect(@subject.quantum_stack_painter).to eq(qsp)
    end
    it 'sets the icon on the label' do
      @subject.quantum_stack_painter = qsp
      expect(@subject.quantum_stack_image_label.icon).to eq('ICON')
    end
  end
end
