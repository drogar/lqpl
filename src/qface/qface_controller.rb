class QfaceController < ApplicationController
  set_model 'QfaceModel'
  set_view 'QfaceView'
  set_close_action :exit

  java_import javax.swing.JFileChooser
  java_import javax.swing.filechooser.FileNameExtensionFilter

  def file_compile_action_performed
    chooser = JFileChooser.new()
    chooser.set_dialog_title "Open LQPL File for Compiling"
    qplfiles = FileNameExtensionFilter.new("LQPL source file", ["qpl"].to_java(:string))
    chooser.set_file_filter(qplfiles)
    chooser.set_current_directory(java.io.File.new(Dir.getwd))
    rval = chooser.show_open_dialog(nil)
    if rval == JFileChooser::APPROVE_OPTION
      fname = chooser.get_selected_file.get_absolute_path
      cmp = Compiler.get_instance
      cmp.compile fname
      model.messages_text = "Compile of #{chooser.get_selected_file.name} was #{cmp.failed ? 'un' : ''}successful\n"
      model.messages_text << cmp.failure_message
      cmp.write_qpo_file if !cmp.failed
    else
      puts "Did not do approve."
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
      server = ServerConnection.instance
      server.send_load_from_file fname
      model.frame_title = "Quantum Emulator - #{base_file_name}"
      model.go_enabled = true
      model.step_enabled = true
      model.spinner_panel_visible = true
      model.button_panel_visible = true
      initialize_sub_controllers

    else
      puts "Did not do approve."
    end
    update_view
  end

  def file_simulate_action_performed

    SimulateResultsController.instance.server_connection = ServerConnection.instance
    SimulateResultsController.instance().set_simulate_results(model.recursion_spinner,StackTranslationController.instance.get_stack_translation)
    SimulateResultsController.instance.open
  end

  def viewClassicalStackMI_action_performed
    ClassicalStackController.instance.toggle_visibility
    model.view_menu_classical_stack_text = ClassicalStackController.instance.visible? ? "Hide Classical Stack" : "Show Classical Stack"
    update_view
  end

  def viewDumpMI_action_performed
    DumpController.instance.toggle_visibility
    model.view_menu_dump_text = DumpController.instance.visible? ? "Hide Dump" : "Show Dump"
    update_view
  end

  def viewExecutingCodeMI_action_performed
    ExecutableCodeController.instance.toggle_visibility
    model.view_menu_executing_code_text = ExecutableCodeController.instance.visible? ? "Hide Executing Code" : "Show Executing Code"
    update_view
  end

  def viewStackTranslationMI_action_performed
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
    QuantumStackController.instance.server_connection = ServerConnection.instance
    ExecutableCodeController.instance.server_connection = ServerConnection.instance
    ClassicalStackController.instance.server_connection = ServerConnection.instance
    DumpController.instance.server_connection = ServerConnection.instance
    StackTranslationController.instance.server_connection = ServerConnection.instance
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
    model.step_spinner = view_model.step_spinner
  end

  def recursion_spinner_state_changed
    model.recursion_spinner = view_model.recursion_spinner
    update_sub_model_data
  end

  def tree_depth_spinner_state_changed
    model.tree_depth_spinner = view_model.tree_depth_spinner
    update_sub_model_data
  end

  def step_button_action_performed
    sc = ServerConnection.instance
    res = sc.do_step(model.step_spinner,model.recursion_spinner)
    update_sub_model_data
    if res =~ /executed/
      model.go_enabled = false
      model.step_enabled = false
      update_view
    end
  end

  def go_button_action_performed
    sc = ServerConnection.instance
    sc.do_run model.recursion_spinner
    update_sub_model_data
    model.go_enabled = false
    model.step_enabled = false
    update_view
  end
end
