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
  # it "should set the server_connection when given an sc" do
  # 
  #     sc = double('server_connection')
  #     sc.should_receive(:connected?).and_return(true)
  #     @d.lqpl_emulator_server_connection=sc
  #   end
  #   it "should ask the sc for the quantum stack and the st model for a num when given a depth and recursion and st" do
  #     sc = double('server_connection')
  #     sc.should_receive(:connected?).and_return(true)
  #     sc.should_receive(:get_qstack).and_return(QB2WITHBOTTOM)
  # 
  #     st = double("StackTranslation", :nil? => false)
  #     st.should_receive(:reverse_lookup).and_return("p")
  # 
  #     @d.lqpl_emulator_server_connection=sc
  #     @d.set_quantum_stack("5","4",st)
  # 
  #   end
end