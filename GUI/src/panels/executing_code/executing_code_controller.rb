# controller to handle the executing code display
class ExecutingCodeController < PanelController
  set_model 'ExecutingCodeModel'
  set_view 'ExecutingCodeView'
  set_close_action :hide

  def update_data_from_lqpl_model(lqpl_model)
    update_code_pointer lqpl_model.recursion_spinner.int_value
  end

  def update_code_and_code_pointer(recursion_depth)
    model.the_code = lqpl_emulator_server_connection.get_code recursion_depth
    model.the_code_pointer = lqpl_emulator_server_connection.get_codepointer recursion_depth
    model.the_code_was_updated = true
    update_view
  end

  def update_code_pointer(recursion_depth)
    model.the_code_pointer = lqpl_emulator_server_connection.get_codepointer recursion_depth
    model.the_code_was_updated = false
    update_view
  end
end
