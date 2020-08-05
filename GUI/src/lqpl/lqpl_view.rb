require 'lqpl_menu'
require 'quantum_emulator_main_form'

# view  for the main lqpl application
class LqplView < ApplicationView
  set_java_class QuantumEmulatorMainForm

  attr_accessor :the_menu
  attr_reader :main_view_component
  alias the_frame main_view_component

  MODEL_TO_VIEW_MAP = {
    spinner_panel_visible: 'spinner_panel.visible',
    button_panel_visible: 'button_panel.visible',

    step_spinner: 'step_spinner.model.value',
    recursion_spinner: 'recursion_spinner.model.value',
    recursion_multiplier_spinner: 'recursion_multiplier_spinner.model.value',
    tree_depth_spinner: 'tree_depth_spinner.model.value',

    messages_text: 'messages_text_area.text',

    step_enabled: 'step_button.enabled',
    go_enabled: 'go_button.enabled',

    view_menu_classical_stack_enabled: 'the_menu.view_classical_stack.enabled',
    view_menu_dump_enabled: 'the_menu.view_dump.enabled',
    view_menu_executing_code_enabled: 'the_menu.view_executing_code.enabled',
    view_menu_stack_translation_enabled: 'the_menu.view_stack_translation.enabled',

    view_menu_classical_stack_text: 'the_menu.view_classical_stack.text',
    view_menu_dump_text: 'the_menu.view_dump.text',
    view_menu_executing_code_text: 'the_menu.view_executing_code.text',
    view_menu_stack_translation_text: 'the_menu.view_stack_translation.text'
  }.freeze

  raw_mapping :set_title, nil

  def self.model_view_mapper(map_of_model_to_view)
    map_of_model_to_view.each { |model, view| map view: view, model: model }
  end

  model_view_mapper(MODEL_TO_VIEW_MAP)

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
