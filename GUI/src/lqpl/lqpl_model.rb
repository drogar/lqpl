# encoding: utf-8
JInteger = java.lang.Integer

# model for the lqpl main screen.
class LqplModel < ApplicationModel
  attr_accessor :spinner_panel_visible, :button_panel_visible, :step_spinner, :recursion_spinner,
                :recursion_multiplier_spinner, :tree_depth_spinner, :go_enabled, :step_enabled,
                :messages_text, :view_menu_classical_stack_enabled, :view_menu_dump_enabled,
                :view_menu_executing_code_enabled, :view_menu_stack_translation_enabled,
                :view_menu_classical_stack_text, :view_menu_dump_text, :frame_title,
                :view_menu_executing_code_text, :view_menu_stack_translation_text,
                :compiler_connection, :lqpl_server_connection

  def initialize
    init_panels
    init_spinners
    init_buttons
    init_view_menu
    @frame_title = 'Quantum Emulator'
  end

  def init_panels
    @spinner_panel_visible = false
    @button_panel_visible = false
  end

  def init_spinners
    @step_spinner = JInteger.new(1)
    @recursion_spinner = JInteger.new(1)
    @recursion_multiplier_spinner = JInteger.new(10)
    @tree_depth_spinner = JInteger.new(4)
  end

  def init_buttons
    enable_go!(true)
  end

  def init_view_menu
    enable_view_menu_items(false)

    @view_menu_stack_translation_text = 'Hide Stack Translation'
    @view_menu_executing_code_text = 'Hide Executing Code'
    @view_menu_dump_text = 'Hide Dump'
    @view_menu_classical_stack_text = 'Hide Classical Stack'
  end

  def toggle_view_menu(current_command)
    send(LqplModel.symbol_for_view_menu_item(current_command),
         LqplModel.new_view_command(current_command))
  end

  def enable_view_menu_items(value = true)
    self.view_menu_stack_translation_enabled = value
    self.view_menu_dump_enabled = value
    self.view_menu_executing_code_enabled = value
    self.view_menu_classical_stack_enabled = value
  end

  def enable_buttons!(base_file_name)
    self.frame_title = "Quantum Emulator - #{base_file_name}"
    enable_go! true
    self.spinner_panel_visible = true
    self.button_panel_visible = true
    self.messages_text = "#{base_file_name} was loaded."
  end

  def enable_go!(value)
    self.go_enabled = value
    self.step_enabled = value
  end

  def self.toggle_action(current)
    current == 'Show' ? 'Hide' : 'Show'
  end

  def self.new_view_command(current_command)
    toggle_action(current_command[0]) + ' ' + current_command[1, current_command.size - 1].join(' ')
  end

  def self.symbol_for_view_menu_item(current_command)
    which_menu = current_command.drop(1).map(&:downcase).join('_')
    "view_menu_#{which_menu}_text=".to_sym
  end

  def compile(file)
    compiler_connection.compile file.get_absolute_path
    self.messages_text = compiler_connection.success_or_fail_message
  end

  def load_and_enable!(file)
    load_file file.absolute_path
    enable_buttons! file.name
  end

  def load_file(file_path)
    lqpl_server_connection.send_load_from_file(recursion_multiplier_spinner.int_value,
                                               file_path)
    lqpl_server_connection.do_depth_multiple(recursion_multiplier_spinner.int_value)
  end

  def update_recursion_spinner(value)
    self.recursion_spinner = java.lang.Integer.new(value)
    self.messages_text = "Recursion Depth set to #{recursion_spinner}"
    enable_go!(true)
  end

  def execute
    lqpl_server_connection.do_run recursion_spinner.int_value
    enable_go! false
  end

  def do_step
    res = lqpl_server_connection.do_step(step_spinner.int_value, recursion_spinner.int_value)
    enable_go! !(res =~ /executed/)
  end

  def update_recursion_multiplier_spinner(value)
    self.recursion_multiplier_spinner =
      java.lang.Integer.new(value)
    lqpl_server_connection.do_depth_multiple(recursion_multiplier_spinner.int_value)
    self.messages_text = "Recursion Multiplier set to #{recursion_multiplier_spinner}"
    enable_go!(true)
  end
end
