# encoding: utf-8
require 'abstract_descriptor_painter'

# Zero value painter
class ZeroDescriptorPainter < AbstractDescriptorPainter
  def my_colour
    Color.black
  end
end
