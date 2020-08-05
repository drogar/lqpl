# Swing component to display the executing code
class ExecutingCodeForm < STFrame
  attr_accessor :code_tabbed_pane

  def initialize
    super('Executing Code', set_bounds: Rectangle.new(10, 330, 250, 400))
    @code_tabbed_pane = TabbedPane.new
    self.content_pane = @code_tabbed_pane
  end
end
