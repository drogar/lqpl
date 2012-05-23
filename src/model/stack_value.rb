

class StackValue < StackDescriptor

  PATTERN=Regexp.new /^<Value>(<number>)?((0?\.\d*)|(1\.0*))(<\/number>)?<\/Value>$/

  def initialize(in_string)
    matc = PATTERN.match in_string
    if matc
      @value = matc[2]
    else
      raise StackDescriptorInvalidCreate, in_string
    end
  end

  # PaintMe interface overrides


  def paintmeAtPoint(g,p,center)
    g.set_color(Color.blue)
    #e = Ellipse2Dd.new(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.fill_oval(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.set_color(Color.black);
    g.draw_oval(center.x-5.0, center.y-5.0, 10.0, 10.0);
    g.draw_string("#{@value}", center.x-20.0, center.y+20.0)
 end

 alias :paintme_at_point :paintmeAtPoint

  # End PaintMe interface
end