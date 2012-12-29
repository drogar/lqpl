class StackTranslationController < ApplicationController
  set_model 'StackTranslationModel'
  set_view 'StackTranslationView'
  set_close_action :hide

  def set_data_from_lqpl_model(lqpl_model)
    set_stack_translation(lqpl_model.tree_depth_spinner, lqpl_model.recursion_spinner)
  end
  
  def set_stack_translation(tree_depth, recursion_level)
    set_stack_translation_data(lqpl_emulator_server_connection.get_stack_translation(tree_depth,recursion_level))
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
