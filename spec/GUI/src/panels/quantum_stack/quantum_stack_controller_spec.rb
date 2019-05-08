require 'specdata/quantum_stack_data'
require 'quantum_stack_controller'
require 'quantum_stack_model'

describe QuantumStackController do
  before(:each) do
    SwingRunner.on_edt do
      @qsc = QuantumStackController.instance
    end
  end

  it 'should return true for update_on_lqpl_model_trim' do
    SwingRunner.on_edt do
      expect(@qsc.update_on_lqpl_model_trim).to be true
    end
  end

  describe :update_data_from_lqpl_model do
    let(:lqpl_model) { double('LqplModel') }
    let(:stc) { double('StackTranslationController') }
    let(:td_spinner) { double('Spinner', int_value: 1) }
    let(:r_spinner) { double('Spinner', int_value: 2) }
    before(:each) do
      allow(@qsc).to receive(:set_quantum_stack)
      allow(lqpl_model).to receive(:tree_depth_spinner) { td_spinner }
      allow(lqpl_model).to receive(:recursion_spinner) { r_spinner }
      allow(StackTranslationController).to receive(:instance) { stc }
      allow(stc).to receive(:stack_translation) { :stack_t }
    end
    it 'delegates to set_quantum_stack' do
      @qsc.update_data_from_lqpl_model(lqpl_model)
      expect(lqpl_model).to have_received(:tree_depth_spinner)
      expect(lqpl_model).to have_received(:recursion_spinner)
      expect(StackTranslationController).to have_received(:instance)
      expect(stc).to have_received(:stack_translation)
      expect(@qsc).to have_received(:set_quantum_stack).with(1, 2, :stack_t)
    end
  end
end
