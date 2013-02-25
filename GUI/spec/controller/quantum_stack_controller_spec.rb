require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_controller'
require 'src/panels/quantum_stack/quantum_stack_model'

describe QuantumStackController do
  before(:each) do
    @d = QuantumStackController.instance
  end
  
  it "should return true for update_on_lqpl_model_trim" do
    @d.update_on_lqpl_model_trim.should be_true
  end
end