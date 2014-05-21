# encoding: utf-8
# Date node painter
class DataDescriptorPainter < AbstractDescriptorPainter
  def my_colour
    Color.magenta
  end

  def my_shape(point)
    Rectangle2D::Double.new(point.x - half_node_size, point.y - half_node_size,
                            node_size, node_size)
  end
end
