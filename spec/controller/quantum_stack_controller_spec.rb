# encoding: UTF-8
require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'
require 'GUI/src/panels/quantum_stack/quantum_stack_controller'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'

describe QuantumStackController do
  before(:each) do
    SwingRunner.on_edt do
      @d = QuantumStackController.instance
    end
  end

  it 'should return true for update_on_lqpl_model_trim' do
    SwingRunner.on_edt do
      expect(@d.update_on_lqpl_model_trim).to be true
    end
  end
end