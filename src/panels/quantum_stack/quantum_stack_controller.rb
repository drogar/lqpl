class QuantumStackController < ApplicationController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end

  def set_quantum_stack(tree_depth,recursion_depth,stack_trans)
    model.stack_translation = stack_trans
    model.quantum_stack =  @server_connection.get_qstack tree_depth,recursion_depth
    update_view
  end
end
