class ZeroDescriptorModel <AbstractDescriptorModel

  def initialize(in_string="<Zero/>")
    raise ModelCreateError, in_string if in_string != "<Zero/>"
    @value = "0"
  end

  def length
    0
  end



end