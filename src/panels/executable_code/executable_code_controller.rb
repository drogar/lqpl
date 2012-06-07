class ExecutableCodeController < ApplicationController
  set_model 'ExecutableCodeModel'
  set_view 'ExecutableCodeView'
  set_close_action :dispose

  def set_code_and_code_pointer(recursion_depth)
    sc = ServerConnection.instance
    model.the_code = sc.loaded_code recursion_depth
    model.the_code_pointer = sc.code_pointer recursion_depth
    model.the_code_was_updated = true
    update_view
  end

  def set_code_pointer(recursion_depth)
    sc = ServerConnection.instance
    model.the_code_pointer = sc.code_pointer recursion_depth
    model.the_code_was_updated = false
    update_view
  end
end
