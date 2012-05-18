class StackZero <StackDescriptor
  def initialize(in_string="<Zero/>")
    if in_string != "<Zero/>"
      raise StackDescriptorInvalidCreate, in_string
    end
  end

  # PaintMe interface overrides


  def paintmeAtPoint(g,p,center)
    g.set_color(Color.green)
    #e = Ellipse2Dd.new(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.fill_oval(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.set_color(Color.black);
    g.draw_oval(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.draw_string("#{@value}", center.x-20.0, center.y+20.0)


  end
  # End PaintMe interface

end