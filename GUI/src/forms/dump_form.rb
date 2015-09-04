# encoding: utf-8
require 'scrollable_label'

# Swing component to display the dump data
class DumpForm < ScrollableLabel
  attr_accessor :dump_text

  def initialize
    super('Dump', Rectangle.new(430, 670, 600, 215))
  end

  def dump_text=(new_text)
    the_scrolling_label.text = new_text
  end

  def dump_text
    the_scrolling_label.text
  end
end
