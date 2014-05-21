# encoding: utf-8
# controller for the stack translation panel
class StackTranslationController < PanelController
  set_model 'StackTranslationModel'
  set_view 'StackTranslationView'
  set_close_action :hide

  def update_data_from_lqpl_model(lqpl_model)
    update_stack_translation(lqpl_model.tree_depth_spinner.int_value,
                             lqpl_model.recursion_spinner.int_value)
  end

  def update_stack_translation(tree_depth, recursion_level)
    update_stack_translation_data(lqpl_emulator_server_connection
      .get_stack_translation(recursion_level, tree_depth))
  end

  def update_stack_translation_data(stack_translation_data)
    model.stack_translation = stack_translation_data
    update_view
  end

  def stack_translation_text
    model.text
  end

  def stack_translation
    model
  end
end
