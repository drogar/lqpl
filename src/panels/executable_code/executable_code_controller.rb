class ExecutableCodeController < ApplicationController
  set_model 'ExecutableCodeModel'
  set_view 'ExecutableCodeView'
  set_close_action :hide


  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end


  def set_code_and_code_pointer(recursion_depth)
    model.the_code = @server_connection.loaded_code recursion_depth
    model.the_code_pointer = @server_connection.code_pointer recursion_depth
    model.the_code_was_updated = true
    update_view
  end

  def set_code_pointer(recursion_depth)
    model.the_code_pointer = @server_connection.code_pointer recursion_depth
    model.the_code_was_updated = false
    update_view
  end

  def toggle_visibility
    visible? ? hide : show
  end

end
