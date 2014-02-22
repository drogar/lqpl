# encoding: utf-8
# Class to handle the panel holding the quantum stack
class QuantumStackPanel < ScrollPane
  attr_accessor :quantum_stack_painter
  attr_accessor :quantum_stack_image_label

  def initialize
    super
    java_import javax.swing.JLabel
    # use actual JLabel so java understands it in setting the viewport
    @quantum_stack_image_label = JLabel.new
    self.viewport_view = @quantum_stack_image_label
    self.background = Color.white
  end

  def preferred_size
    layout_size = super.preferred_size
    max = [layout_size.width, layoutSize.height].max
    Dimension[max + 800, max + 600]
  end

  def quantum_stack_painter=(qsp)
    @quantum_stack_painter = qsp
    @quantum_stack_image_label.icon = qsp.image_of_model
    repaint
  end
end
