require 'abstract_descriptor_painter'

# Data node painter
class DataDescriptorPainter < AbstractDescriptorPainter
  def my_colour
    Color.magenta
  end

  def my_shape(point)
    Rectangle2D::Double.new(my_shape_x(point), my_shape_y(point),
                            node_size, node_size)
  end
end
