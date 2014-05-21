# encoding: utf-8
# add offset copy to point
class CopyPoint
  def self.copy_with_x_and_y_offset(point, xoffset, yoffset)
    Point.new(point.x + (xoffset || 0.0), point.y + (yoffset || 0))
  end
end
