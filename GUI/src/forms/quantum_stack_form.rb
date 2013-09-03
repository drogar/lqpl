class QuantumStackForm < STFrame

  attr_accessor :quantum_stack_panel

  def initialize
    super("Quantum Stack", set_bounds: Rectangle.new(430,10,600,640))
    @quantum_stack_panel = QuantumStackPanel.new
    self.content_pane = @quantum_stack_panel
  end

end