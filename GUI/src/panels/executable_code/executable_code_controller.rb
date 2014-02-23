class ExecutableCodeController < PanelController
  set_model 'ExecutableCodeModel'
  set_view 'ExecutableCodeView'
  set_close_action :hide


  def update_data_from_lqpl_model(lqpl_model)
    set_code_pointer  lqpl_model.recursion_spinner
  end
  
  def set_code_and_code_pointer(recursion_depth)
    model.the_code = lqpl_emulator_server_connection.loaded_code recursion_depth
    model.the_code_pointer = lqpl_emulator_server_connection.code_pointer recursion_depth
    model.the_code_was_updated = true
    update_view
  end

  def set_code_pointer(recursion_depth)
    model.the_code_pointer = lqpl_emulator_server_connection.code_pointer recursion_depth
    model.the_code_was_updated = false
    update_view
  end


end
