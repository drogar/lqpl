# encoding: utf-8
# add offset copy to point
class Point
  def copy_with_x_and_y_offset(xoffset, yoffset)
    Point.new(x + (xoffset || 0.0), y + (yoffset || 0))
  end
end
