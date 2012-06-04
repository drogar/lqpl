class ExecutableCodeController < ApplicationController
  set_model 'ExecutableCodeModel'
  set_view 'ExecutableCodeView'
  set_close_action :dispose

  def set_code_and_code_pointer
    sc = ServerConnection.instance
    model.the_code = sc.loaded_code
    model.the_code_pointer = sc.code_pointer
    model.the_code_was_updated = true
    update_view
  end

  def set_code_pointer
    sc = ServerConnection.instance
    model.the_code_pointer = sc.code_pointer
    model.the_code_was_updated = false
    update_view
  end
end
