require 'abstract_descriptor_painter'

# Zero value painter
class ZeroDescriptorPainter < AbstractDescriptorPainter
  def my_colour
    Color.black
  end
end
