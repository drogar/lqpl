class Point
  def copy_with_x_and_y_offset(xoffset,yoffset)
    Point.new(self.x+xoffset, self.y+yoffset)
  end
end