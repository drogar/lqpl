class StackZero <StackDescriptor
  def initialize(in_string="<Zero/>")
    if in_string != "<Zero/>"
      raise StackDescriptorInvalidCreate, in_string
    end
  end


end