class QfaceModel
  attr_accessor :control_panel_visible
  attr_accessor :step_spinner, :recursion_spinner, :tree_depth_spinner
  attr_accessor :go_enabled, :step_enabled

  attr_accessor :view_menu_classical_stack_enabled,:view_menu_dump_enabled,:view_menu_executing_code_enabled,:view_menu_stack_translation_enabled
  attr_accessor :view_menu_classical_stack_text,:view_menu_dump_text,:view_menu_executing_code_text,:view_menu_stack_translation_text

  def initialize
    @control_panel_visible = false
    @step_spinner = java.lang.Integer.new(1)
    @recursion_spinner = java.lang.Integer.new(10)
    @tree_depth_spinner = java.lang.Integer.new(4)
    @go_enabled = true
    @step_enabled = true

    @view_menu_stack_translation_enabled = false
    @view_menu_dump_enabled = false
    @view_menu_executing_code_enabled = false
    @view_menu_classical_stack_enabled = false

    @view_menu_stack_translation_text = "Hide Stack Translation"
    @view_menu_executing_code_text = "Hide Executing Code"
    @view_menu_dump_text = "Hide Dump"
    @view_menu_classical_stack_text = "Hide Classical Stack"
  end


end
