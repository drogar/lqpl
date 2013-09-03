class QuantumStackController < PanelController
  set_model 'QuantumStackModel'
  set_view 'QuantumStackView'

  def set_data_from_lqpl_model(lqpl_model)
    set_quantum_stack(lqpl_model.tree_depth_spinner,
                      lqpl_model.recursion_spinner,
                      StackTranslationController.instance.get_stack_translation)
  end
  
  def set_quantum_stack(tree_depth,recursion_depth,stack_trans)
    model.stack_translation = stack_trans
    model.quantum_stack =  
      lqpl_emulator_server_connection.get_qstack(tree_depth,
                                                 recursion_depth)
    update_view
  end
    
  def update_on_lqpl_model_trim
    true
  end
end
