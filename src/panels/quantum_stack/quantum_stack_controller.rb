class QuantumStackController < ApplicationController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  def set_quantum_stack(recursion_depth)
    sc = ServerConnection.instance
    model.quantum_stack = sc.get_qstack recursion_depth
    update_view
  end
end
