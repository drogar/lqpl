# Main controller for the main controller class
class LqplController < ApplicationController
  DIALOGS = [AboutController, SimulateResultsController].freeze
  SUBS = [StackTranslationController, ClassicalStackController, DumpController,
          ExecutingCodeController, QuantumStackController].freeze
  attr_accessor :cmp, :sub_controllers_handler, :dialogs_handler

  def self.set_up_controller
    set_model 'LqplModel'
    set_view 'LqplView'
    set_close_action :close
    LqplMenu.prepare_menu_actions(->(opts) { add_listener(opts) })
  end

  set_up_controller

  def close
    dialogs_handler&.dispose_all
    sub_controllers_handler&.dispose_all
    ExitHandler.instance.close_servers
    super
  end

  def load(*)
    @cmp = CompilerServerConnection.get_instance
    # @cmp.connect
    model.compiler_connection = @cmp

    # lqpl_emulator_server_connection.connect
    model.lqpl_server_connection = lqpl_emulator_server_connection
    @sub_controllers_handler = LqplSubsHandler.new(SUBS)
    @dialogs_handler = LqplSubsHandler.new(DIALOGS)
  end

  def file_exit_action_performed
    close
  end

  def help_about_action_performed
    AboutController.instance.handleAbout(nil)
  end

  def file_compile_action_performed
    LqplFileChooser.open_and_compile(my_frame, model)
    update_view
  end

  def file_load_action_performed
    LqplFileChooser.open_and_load_qpl(model) && initialize_sub_controllers
    update_view
  end

  def file_simulate_action_performed
    SimulateResultsController.instance
                             .set_simulate_results(recursion_depth,
                                                   StackTranslationController.instance.stack_translation)
    SimulateResultsController.instance.open
  end

  def recursion_depth
    model.recursion_spinner.int_value
  end

  def view_sub_panel_action_performed(event)
    command_and_sub_panel = event.action_command.scan(/\w+/)
    PanelController.controller_from_name(command_and_sub_panel).instance.toggle_visibility
    model.toggle_view_menu(command_and_sub_panel)
    update_view
  end

  def initialize_sub_controllers
    ExecutingCodeController.instance.update_code_and_code_pointer recursion_depth
    sub_controllers_handler.update_and_open(model)
    model.enable_view_menu_items
  end

  def step_spinner_state_changed
    model.step_spinner = java.lang.Integer.new(view_model.step_spinner)
  end

  def recursion_spinner_state_changed
    model.update_recursion_spinner(view_model.recursion_spinner)
    update_all
  end

  def recursion_multiplier_spinner_state_changed
    model.update_recursion_multiplier_spinner(view_model.recursion_multiplier_spinner)
    update_all
  end

  def update_all
    update_view
    sub_controllers_handler.update_all(model)
  end

  def tree_depth_spinner_state_changed
    model.tree_depth_spinner = model_tree_depth_spinner_value
    model.messages_text = message_text_tree_depth_spinner
    update_all
  end

  def model_tree_depth_spinner_value
    java.lang.Integer.new(view_model.tree_depth_spinner)
  end

  def message_text_tree_depth_spinner
    "Tree Depth set to #{model.tree_depth_spinner}"
  end

  def step_button_action_performed
    model.do_step
    update_all
  end

  def go_button_action_performed
    model.execute
    update_all
  end

  def trim_button_action_performed
    model.messages_text = lqpl_emulator_server_connection.do_trim
    sub_controllers_handler.update_on_trim(model)
    update_view
  end
end
