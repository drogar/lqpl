class QuantumStackController < ApplicationController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  def set_quantum_stack(tree_depth,recursion_depth,stack_trans)
    model.stack_translation = stack_trans
    model.quantum_stack =  lqpl_emulator_server_connection.get_qstack tree_depth,recursion_depth
    update_view
  end
end
