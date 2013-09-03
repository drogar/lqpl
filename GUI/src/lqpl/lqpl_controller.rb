
java_import java.awt.event.WindowEvent
require 'dialogs/about/about_controller'
require 'exit_handler'

class LqplController < ApplicationController
  set_model 'LqplModel'
  set_view 'LqplView'
  set_close_action :close
  
  attr_accessor :cmp
  attr_accessor :sub_controllers
  attr_accessor :dialogs
  attr_accessor :qpl_dialog
  
  def self.set_file_menu_actions
    { "the_menu.file_compile" => "file_compile", 
      "the_menu.file_load" => "file_load",
      "the_menu.file_simulate" => "file_simulate"}.each do |k,v|
        add_listener :type => :action, :components => {k => v}
      end
  end
  
  def self.set_view_menu_actions
    [ "the_menu.view_classical_stack",
      "the_menu.view_dump" ,
      "the_menu.view_executing_code" ,
      "the_menu.view_stack_translation"].each do |k|
        add_listener :type => :action, :components => {k => "view_sub_panel"}
      end
  end
  
  def self.set_up_exit_and_about
    on_mac do 
      java_import com.apple.eawt.Application
      Application.application.about_handler = AboutController.instance
      Application.application.quit_handler = ExitHandler.instance
    end
  
    not_on_mac do
      { "the_menu.file_exit" => "file_exit",
        "the_menu.help_about" => "help_about"}.each do |k,v|
          add_listener :type => :action, :components => {k => v}
      end
    end
  end
      
  set_file_menu_actions
  set_view_menu_actions
  set_up_exit_and_about
  

  def close
    all_controllers_dispose
    ExitHandler.instance.close_servers
    super
  end
  
  def load(*args)

    @cmp = CompilerServerConnection.get_instance
    @cmp.connect

    self.lqpl_emulator_server_connection.connect
    @sub_controllers = [QuantumStackController, ClassicalStackController, DumpController, ExecutableCodeController,
      StackTranslationController].inject([]) {|memo,controller| memo << controller.instance}
    @dialogs = [AboutController, SimulateResultsController].inject([]) { |mem, var|  mem << var.instance}
  end

  def file_exit_action_performed
    close
  end

  def help_about_action_performed
    AboutController.instance.handleAbout(nil)
  end

  def file_compile_action_performed
    chooser = JFileChooser.lqpl_source_file_opener
    if chooser.show_open_dialog(self.my_frame) == JFileChooser::APPROVE_OPTION
      @cmp.compile_and_write_qpo chooser.get_selected_file.get_absolute_path
      model.messages_text = @cmp.success_or_fail_message(chooser.get_selected_file.name)
    else
      model.messages_text  = "Compile action cancelled."
    end
    update_view
  end

  def file_load_action_performed
    @qpl_dialog = JFileChooser.lqpl_assembled_file_opener
    if @qpl_dialog.show_open_dialog(nil) == JFileChooser::APPROVE_OPTION
      load_file @qpl_dialog.selected_file.absolute_path
      model.set_title_and_enable @qpl_dialog.selected_file.name
      initialize_sub_controllers
    else
      model.messages_text =  "QPO file load cancelled."
    end
    update_view
  end

  def load_file(file_path)
    self.lqpl_emulator_server_connection.send_load_from_file(model.recursion_multiplier_spinner, file_path)
    self.lqpl_emulator_server_connection.send_set_depth_multiplier(model.recursion_multiplier_spinner)
  end
      
  def all_controllers_dispose
    @dialogs.each {|d| d.dispose} if @dialogs
    @sub_controllers.each{|sc| sc.dispose} if @sub_controllers
  end
  
  def file_simulate_action_performed
    SimulateResultsController.instance.set_simulate_results(model.recursion_spinner,StackTranslationController.instance.get_stack_translation)
    SimulateResultsController.instance.open
  end

  def view_sub_panel_action_performed(e)
    command_and_sub_panel = e.action_command.scan(/\w+/)
    PanelController::controller_from_name(command_and_sub_panel).instance.toggle_visibility
    model.toggle_view_menu(command_and_sub_panel)
    update_view
  end
 
  def initialize_sub_controllers
    ExecutableCodeController.instance.set_code_and_code_pointer  model.recursion_spinner

    update_sub_model_data
    open_sub_panels
    model.enable_view_menu_items
  end

  def open_sub_panels
    @sub_controllers.each {|sc| sc.open}
  end

  def update_sub_model_data
    @sub_controllers.each {|sc| sc.set_data_from_lqpl_model(model)}
  end

  def step_spinner_state_changed
    model.step_spinner = java.lang.Integer.new(view_model.step_spinner)
  end

  def recursion_spinner_state_changed
    model.recursion_spinner = java.lang.Integer.new(view_model.recursion_spinner)
    model.messages_text =  "Recursion Depth set to #{model.recursion_spinner}"
    enable_and_update true
  end

  def recursion_multiplier_spinner_state_changed
    model.recursion_multiplier_spinner = java.lang.Integer.new("#{view_model.recursion_multiplier_spinner}")
    lqpl_emulator_server_connection.send_set_depth_multiplier(model.recursion_multiplier_spinner)
    model.messages_text =  "Recursion Multiplier set to #{model.recursion_multiplier_spinner}"
    enable_and_update true
  end
  
  def enable_and_update(enabled)
    model.go_enabled = enabled
    model.step_enabled = enabled
    update_view
    update_sub_model_data
  end
  

  def tree_depth_spinner_state_changed
    model.tree_depth_spinner = java.lang.Integer.new(view_model.tree_depth_spinner)
    model.messages_text =  "Tree Depth set to #{model.tree_depth_spinner}"
    update_view
    update_sub_model_data
  end

  def step_button_action_performed
    res = self.lqpl_emulator_server_connection.do_step(model.step_spinner,model.recursion_spinner)
    enable_and_update !(res =~ /executed/)
  end

  def go_button_action_performed
    self.lqpl_emulator_server_connection.do_run model.recursion_spinner
    enable_and_update false
  end

  def trim_button_action_performed
    model.messages_text = self.lqpl_emulator_server_connection.do_trim
    @sub_controllers.each {|sc| sc.set_data_from_lqpl_model(model) if sc.update_on_lqpl_model_trim}
    update_view
  end
end
