# encoding: utf-8
# Swing for to display the classical stack
class ClassicalStackForm < ScrollableLabel
  attr_accessor :classical_stack_text

  def initialize
    super('Classical Stack', Rectangle.new(270, 330, 150, 400))
  end

  def classical_stack_text=(new_text)
    the_scrolling_label.text = new_text
  end

  def classical_stack_text
    the_scrolling_label.text
  end
end
