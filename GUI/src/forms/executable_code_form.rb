class ExecutableCodeForm < STFrame
  
  attr_accessor :code_tabbed_pane
  
  def initialize
    super("Executing Code")
    self.bounds = Rectangle.new(10, 330, 250, 400)
    @code_tabbed_pane = TabbedPane.new
    self.content_pane = @code_tabbed_pane
  end
  
  
end