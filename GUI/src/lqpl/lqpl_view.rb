require 'lqpl_menu'
require 'quantum_emulator_main_form'

# view  for the main lqpl application
class LqplView < ApplicationView
  set_java_class QuantumEmulatorMainForm

  attr_accessor :the_menu
  attr_reader :main_view_component
  alias the_frame main_view_component

  map view: 'spinner_panel.visible', model: :spinner_panel_visible
  map view: 'button_panel.visible', model: :button_panel_visible

  map view: 'step_spinner.model.value', model: :step_spinner
  map view: 'recursion_spinner.model.value', model: :recursion_spinner
  map view: 'recursion_multiplier_spinner.model.value', model: :recursion_multiplier_spinner
  map view: 'tree_depth_spinner.model.value', model: :tree_depth_spinner

  map view: 'messages_text_area.text', model: :messages_text

  map view: 'step_button.enabled', model: :step_enabled
  map view: 'go_button.enabled', model: :go_enabled

  map view: 'the_menu.view_classical_stack.enabled', model: :view_menu_classical_stack_enabled
  map view: 'the_menu.view_dump.enabled', model: :view_menu_dump_enabled
  map view: 'the_menu.view_executing_code.enabled', model: :view_menu_executing_code_enabled
  map view: 'the_menu.view_stack_translation.enabled', model: :view_menu_stack_translation_enabled

  map view: 'the_menu.view_classical_stack.text', model: :view_menu_classical_stack_text
  map view: 'the_menu.view_dump.text', model: :view_menu_dump_text
  map view: 'the_menu.view_executing_code.text', model: :view_menu_executing_code_text
  map view: 'the_menu.view_stack_translation.text', model: :view_menu_stack_translation_text

  raw_mapping :set_title, nil

  def load(*)
    @the_menu = LqplMenu.new(self)
  end

  def make_menu_bar(mbar)
    main_view_component.setJMenuBar(mbar)
  end

  def set_title(model, _transfer)
    main_view_component.title = model.frame_title
  end
end
