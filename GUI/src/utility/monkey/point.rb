class Point
  def copy_with_x_and_y_offset(xoffset,yoffset)
    Point.new(self.x+(xoffset||0.0), self.y+(yoffset||0))
  end
end