# encoding: utf-8
java_import java.awt.event.WindowEvent
require 'dialogs/about/about_controller'
require 'dialogs/simulate_results/simulate_results_controller'
require 'exit_handler'

# Main controller for the main controller class
class LqplController < ApplicationController
  set_model 'LqplModel'
  set_view 'LqplView'
  set_close_action :close
  DIALOGS = [AboutController,  SimulateResultsController]
  SUBS = [QuantumStackController, ClassicalStackController, DumpController,
          ExecutableCodeController, StackTranslationController]
  attr_accessor :cmp, :sub_controllers_handler, :dialogs_handler, :qpl_dialog

  LqplMenu.set_menu_actions(->(opts) { add_listener(opts) })

  def close
    all_controllers_dispose
    ExitHandler.instance.close_servers
    super
  end

  def load(*args)
    @cmp = CompilerServerConnection.get_instance
    # @cmp.connect
    model.compiler_server_connection = @cmp

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
    chooser = JFileChooser.lqpl_source_file_opener
    if chooser.show_open_dialog(my_frame) == JFileChooser::APPROVE_OPTION
      model.compile(chooser.get_selected_file)
    else
      model.messages_text  = 'Compile action cancelled.'
    end
    update_view
  end

  def file_load_action_performed
    @qpl_dialog = JFileChooser.lqpl_assembled_file_opener
    if @qpl_dialog.show_open_dialog(nil) == JFileChooser::APPROVE_OPTION
      model.load_and_enable! @qpl_dialog.selected_file
      initialize_sub_controllers
    else
      model.messages_text =  'QPO file load cancelled.'
    end
    update_view
  end

  def all_controllers_dispose
    dialogs_handler.dispose_all
    @sub_controllers_handler.dispose_alleach { |sc| sc.dispose } if @sub_controllers
  end

  def file_simulate_action_performed
    SimulateResultsController.instance
      .set_simulate_results(model.recursion_spinner,
                            StackTranslationController.instance.get_stack_translation)
    SimulateResultsController.instance.open
  end

  def view_sub_panel_action_performed(e)
    command_and_sub_panel = e.action_command.scan(/\w+/)
    PanelController.controller_from_name(command_and_sub_panel).instance.toggle_visibility
    model.toggle_view_menu(command_and_sub_panel)
    update_view
  end

  def initialize_sub_controllers
    ExecutableCodeController.instance.set_code_and_code_pointer  model.recursion_spinner
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
    update_recursion_multiplier_spinner("#{view_model.recursion_multiplier_spinner}")
    update_all
  end

  def update_all
    update_view
    sub_controllers_handler.update_all(model)
  end

  def tree_depth_spinner_state_changed
    model.tree_depth_spinner = java.lang.Integer.new(view_model.tree_depth_spinner)
    model.messages_text =  "Tree Depth set to #{model.tree_depth_spinner}"
    update_all
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
