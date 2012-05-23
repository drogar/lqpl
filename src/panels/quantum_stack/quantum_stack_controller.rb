class QuantumStackController < ApplicationController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  def update_qstack
    sc = ServerConnection.instance
    model.qstack = sc.get_qstack
    update_view
  end
end
