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
end
