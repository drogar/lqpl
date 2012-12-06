java_import javax.swing.JFileChooser
java_import javax.swing.filechooser.FileNameExtensionFilter
java_import java.awt.event.WindowEvent
require 'dialogs/about/about_controller'
require 'exit_handler'

class LqplController < ApplicationController
  set_model 'LqplModel'
  set_view 'LqplView'
  set_close_action :exit

  {"the_menu.file_compile" => "file_compile", "the_menu.file_load" => "file_load",
    "the_menu.file_simulate" => "file_simulate","the_menu.view_classical_stack" => "view_classical_stack",
    "the_menu.view_dump" => "view_dump","the_menu.view_executing_code" => "view_executing_code",
    "the_menu.view_stack_translation" => "view_stack_translation"}.each do |k,v|
      add_listener :type => :action, :components => {k => v}
    end

  case RbConfig::CONFIG["host_os"]
  when /darwin/i # OSX specific code
    java_import com.apple.eawt.Application
    Application.application.about_handler = AboutController.instance
    Application.application.quit_handler = ExitHandler.instance
   #when /^win|mswin/i # Windows specific code
   #when /linux/i # Linux specific code
  else # Windows and Linux
    add_listener :type => :action, :components => {"the_menu.file_exit" => "file_exit"}
    add_listener :type => :action, :components => {"the_menu.help_about" => "help_about"}
  end

  def load(*args)
    cmp = CompilerServerConnection.get_instance
    cmp.connect
    @lqpl_emulator_server_connection = LqplEmulatorServerConnection.get_instance
    @lqpl_emulator_server_connection.connect
  end

  def file_exit_action_performed
    ExitHandler.instance.close_servers
  end

  def help_about_action_performed
    AboutController.instance.open
  end

  def file_compile_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Open LQPL File for Compiling"
    qplfiles = FileNameExtensionFilter.new("LQPL source file", ["qpl"].to_java(:string))
    chooser.set_file_filter(qplfiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    if rval == JFileChooser::APPROVE_OPTION
      fname = chooser.get_selected_file.get_absolute_path
      cmp = CompilerServerConnection.get_instance
      cmp.compile fname
      model.messages_text = "Compile of #{chooser.get_selected_file.name} was #{cmp.failed ? 'un' : ''}successful\n"
      model.messages_text << cmp.failure_message
      cmp.write_qpo_file if !cmp.failed
    else
      model.messages_text  = "Compile action cancelled."
    end
    update_view
  end

  def file_load_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Load LQPO (Assembly) File"
    qpofiles = FileNameExtensionFilter.new("LQPL assembled file", ["qpo"].to_java(:string))
    chooser.set_file_filter(qpofiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    if rval == JFileChooser::APPROVE_OPTION
      fname = chooser.selected_file.absolute_path
      base_file_name = chooser.selected_file.name
      @lqpl_emulator_server_connection = LqplEmulatorServerConnection.get_instance
      @lqpl_emulator_server_connection.send_load_from_file(model.recursion_multiplier_spinner, fname)
      model.frame_title = "Quantum Emulator - #{base_file_name}"
      model.go_enabled = true
      model.step_enabled = true
      model.spinner_panel_visible = true
      model.button_panel_visible = true
      model.messages_text = "#{base_file_name} was loaded."
      @lqpl_emulator_server_connection.send_set_depth_multiplier(model.recursion_multiplier_spinner)
      initialize_sub_controllers

    else
      model.messages_text =  "QPO file load cancelled."
    end
    update_view
  end

  def file_simulate_action_performed

    SimulateResultsController.instance.lqpl_emulator_server_connection = LqplEmulatorServerConnection.get_instance
    SimulateResultsController.instance().set_simulate_results(model.recursion_spinner,StackTranslationController.instance.get_stack_translation)
    SimulateResultsController.instance.open
  end

  def view_classical_stack_action_performed
    ClassicalStackController.instance.toggle_visibility
    model.view_menu_classical_stack_text = ClassicalStackController.instance.visible? ? "Hide Classical Stack" : "Show Classical Stack"
    update_view
  end

  def view_dump_action_performed
    DumpController.instance.toggle_visibility
    model.view_menu_dump_text = DumpController.instance.visible? ? "Hide Dump" : "Show Dump"
    update_view
  end

  def view_executing_code_action_performed
    ExecutableCodeController.instance.toggle_visibility
    model.view_menu_executing_code_text = ExecutableCodeController.instance.visible? ? "Hide Executing Code" : "Show Executing Code"
    update_view
  end

  def view_stack_translation_action_performed
    StackTranslationController.instance.toggle_visibility
    model.view_menu_stack_translation_text = StackTranslationController.instance.visible? ? "Hide Stack Translation" : "Show Stack Translation"
    update_view
  end

  def initialize_sub_controllers
    update_sub_controller_scs
    ExecutableCodeController.instance.set_code_and_code_pointer  model.recursion_spinner

    update_sub_model_data
    open_sub_panels
    enable_view_menu_items
  end

  def enable_view_menu_items
    model.view_menu_stack_translation_enabled = true
    model.view_menu_dump_enabled = true
    model.view_menu_executing_code_enabled = true
    model.view_menu_classical_stack_enabled = true
  end

  def update_sub_controller_scs
    QuantumStackController.instance.lqpl_emulator_server_connection = @lqpl_emulator_server_connection
    ExecutableCodeController.instance.lqpl_emulator_server_connection = @lqpl_emulator_server_connection
    ClassicalStackController.instance.lqpl_emulator_server_connection = @lqpl_emulator_server_connection
    DumpController.instance.lqpl_emulator_server_connection = @lqpl_emulator_server_connection
    StackTranslationController.instance.lqpl_emulator_server_connection = @lqpl_emulator_server_connection
  end

  def open_sub_panels
    QuantumStackController.instance.open
    ExecutableCodeController.instance.open
    ClassicalStackController.instance.open
    DumpController.instance.open
    StackTranslationController.instance.open
  end

  def update_sub_model_data
    StackTranslationController.instance.set_stack_translation(model.tree_depth_spinner, model.recursion_spinner)
    QuantumStackController.instance.set_quantum_stack(model.tree_depth_spinner,model.recursion_spinner,StackTranslationController.instance.get_stack_translation)
    ExecutableCodeController.instance.set_code_pointer  model.recursion_spinner
    ClassicalStackController.instance.set_classical_stack(model.tree_depth_spinner, model.recursion_spinner)
    DumpController.instance.set_dump(model.tree_depth_spinner, model.recursion_spinner)
  end

  def step_spinner_state_changed
    model.step_spinner = java.lang.Integer.new(view_model.step_spinner)
  end

  def recursion_spinner_state_changed
    model.recursion_spinner = java.lang.Integer.new(view_model.recursion_spinner)
    model.go_enabled = true
    model.step_enabled = true
    update_view
    update_sub_model_data
  end

  def recursion_multiplier_spinner_state_changed
    model.recursion_multiplier_spinner = java.lang.Integer.new("#{view_model.recursion_multiplier_spinner}")
    lqpl_emulator_server_connection.send_set_depth_multiplier(model.recursion_multiplier_spinner)
    model.go_enabled = true
    model.step_enabled = true
    update_view
    update_sub_model_data
  end

  def tree_depth_spinner_state_changed
    model.tree_depth_spinner = java.lang.Integer.new(view_model.tree_depth_spinner)
    update_sub_model_data
  end

  def step_button_action_performed
    sc = LqplEmulatorServerConnection.instance
    res = sc.do_step(model.step_spinner,model.recursion_spinner)
    update_sub_model_data
    if res =~ /executed/
      model.go_enabled = false
      model.step_enabled = false
      update_view
    end
  end

  def go_button_action_performed
    sc = LqplEmulatorServerConnection.instance
    sc.do_run model.recursion_spinner
    update_sub_model_data
    model.go_enabled = false
    model.step_enabled = false
    update_view
  end

  def trim_button_action_performed
    sc = LqplEmulatorServerConnection.instance
    model.messages_text = sc.do_trim
    QuantumStackController.instance.set_quantum_stack(model.tree_depth_spinner,model.recursion_spinner,StackTranslationController.instance.get_stack_translation)
    DumpController.instance.set_dump(model.tree_depth_spinner, model.recursion_spinner)
    update_view
  end
end
