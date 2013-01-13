class ClassicalStackController < PanelController
  set_model 'ClassicalStackModel'
  set_view 'ClassicalStackView'
  set_close_action :hide

  def set_data_from_lqpl_model(lqpl_model)
    set_classical_stack(lqpl_model.tree_depth_spinner, lqpl_model.recursion_spinner)
  end
  
  def set_classical_stack(tree_depth, recursion_level)
    set_classical_stack_data(lqpl_emulator_server_connection.get_classical_stack(tree_depth,recursion_level))
  end

  def set_classical_stack_data(cs_data)
    model.classical_stack= cs_data
    update_view
  end

  def get_classical_stack_data
    model.classical_stack_text
  end
end
