class QuantumStackController < ApplicationController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  def set_quantum_stack
    sc = ServerConnection.instance
    model.quantum_stack = sc.get_qstack
    update_view
  end
end
