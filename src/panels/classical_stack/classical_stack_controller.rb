class ClassicalStackController < ApplicationController
  set_model 'ClassicalStackModel'
  set_view 'ClassicalStackView'
  set_close_action :hide

  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end

  def set_classical_stack(tree_depth, recursion_level)
    set_classical_stack_data(@server_connection.get_classical_stack(tree_depth,recursion_level))
  end

  def set_classical_stack_data(cs_data)
    model.classical_stack= cs_data
    update_view
  end

  def get_classical_stack_data
    model.classical_stack_text
  end
end
