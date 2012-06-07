class QfaceModel
  attr_accessor :control_panel_visible
  attr_accessor :step_spinner, :recursion_spinner, :tree_depth_spinner
  attr_accessor :go_enabled, :step_enabled

  def initialize
    @control_panel_visible = false
    @step_spinner = java.lang.Integer.new(1)
    @recursion_spinner = java.lang.Integer.new(10)
    @tree_depth_spinner = java.lang.Integer.new(4)
    @go_enabled = true
    @step_enabled = true
  end


end
