class StackTranslationController < ApplicationController
  set_model 'StackTranslationModel'
  set_view 'StackTranslationView'
  set_close_action :hide

  attr_accessor :server_connection

  def server_connection=(sc)
    @server_connection = sc
    @server_connection.connect if !@server_connection.connected?
  end

  def set_stack_translation(tree_depth, recursion_level)
    set_stack_translation_data(@server_connection.get_stack_translation(tree_depth,recursion_level))
  end

  def set_stack_translation_data(stack_translation_data)
    model.stack_translation= stack_translation_data
    update_view
  end

  def get_stack_translation_text
    model.text
  end

  def get_stack_translation
    model
  end
end
