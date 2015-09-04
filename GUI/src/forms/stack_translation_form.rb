# encoding: utf-8
require 'scrollable_label'

# Swing component to display the stack translation structure
class StackTranslationForm < ScrollableLabel
  attr_accessor :stack_translation_text

  def initialize
    super('Stack Translation', Rectangle.new(10, 740, 400, 150))
  end

  def stack_translation_text=(new_text)
    the_scrolling_label.text = new_text
  end

  def stack_translation_text
    the_scrolling_label.text
  end
end
