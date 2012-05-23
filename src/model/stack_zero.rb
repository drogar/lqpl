class StackZero <StackDescriptor
  def initialize(in_string="<Zero/>")
    if in_string != "<Zero/>"
      raise StackDescriptorInvalidCreate, in_string
    end
    @value = "0"

  end

  def length
    0
  end
  # PaintMe interface overrides


  def my_colour
    Color.black
  end

  # End PaintMe interface

end