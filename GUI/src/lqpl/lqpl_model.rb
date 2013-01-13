JInteger = java.lang.Integer

class LqplModel < ApplicationModel
  attr_accessor :spinner_panel_visible, :button_panel_visible
  attr_accessor :step_spinner, :recursion_spinner, :recursion_multiplier_spinner, :tree_depth_spinner
  attr_accessor :go_enabled, :step_enabled

  attr_accessor :messages_text

  attr_accessor :view_menu_classical_stack_enabled,:view_menu_dump_enabled,:view_menu_executing_code_enabled,:view_menu_stack_translation_enabled
  attr_accessor :view_menu_classical_stack_text,:view_menu_dump_text,:view_menu_executing_code_text,:view_menu_stack_translation_text

  attr_accessor :frame_title

  def initialize
    init_panels
    init_spinners
    init_buttons
    init_view_menu
    @frame_title = "Quantum Emulator"
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
    @go_enabled = true
    @step_enabled = true
  end
    
  def init_view_menu
    @view_menu_stack_translation_enabled = false
    @view_menu_dump_enabled = false
    @view_menu_executing_code_enabled = false
    @view_menu_classical_stack_enabled = false

    @view_menu_stack_translation_text = "Hide Stack Translation"
    @view_menu_executing_code_text = "Hide Executing Code"
    @view_menu_dump_text = "Hide Dump"
    @view_menu_classical_stack_text = "Hide Classical Stack"
  end

  def toggle_view_menu(current_command)
    self.send(LqplModel::symbol_for_view_menu_item(current_command), LqplModel::new_view_command(current_command))
  end
  
  def enable_view_menu_items
    self.view_menu_stack_translation_enabled = true
    self.view_menu_dump_enabled = true
    self.view_menu_executing_code_enabled = true
    self.view_menu_classical_stack_enabled = true
  end
  
  def set_title_and_enable(base_file_name)
    self.frame_title = "Quantum Emulator - #{base_file_name}"
    self.go_enabled = true
    self.step_enabled = true
    self.spinner_panel_visible = true
    self.button_panel_visible = true
    self.messages_text = "#{base_file_name} was loaded."
  end
  
  def self.toggle_action(current)
    return "Hide" if current == "Show"
    "Show"
  end
  
  def self.new_view_command(current_command)
    toggle_action(current_command[0])+" "+current_command[1,current_command.size-1].join(" ")
  end
  
  def self.symbol_for_view_menu_item(current_command)
    which_menu=current_command.drop(1).collect(&:downcase).join("_")
    "view_menu_#{which_menu}_text=".to_sym
  end
end
