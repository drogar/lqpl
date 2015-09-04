# encoding: utf-8
require 'quantum_stack_panel'

# Swing component to paint the quantum stack in
class QuantumStackForm < STFrame
  attr_accessor :quantum_stack_panel

  def initialize
    super('Quantum Stack', set_bounds: Rectangle.new(430, 10, 600, 640))
    @quantum_stack_panel = QuantumStackPanel.new
    self.content_pane = @quantum_stack_panel
  end
end
