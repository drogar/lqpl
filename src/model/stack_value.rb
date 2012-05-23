

class StackValue < StackDescriptor

  PATTERN=Regexp.new /^<Value>(<number>)?((0?\.\d*)|(1\.0*))(<\/number>)?<\/Value>$/

  def initialize(in_string)
    matc = PATTERN.match in_string
    if matc
      @value = matc[2]
      @name = nil
    else
      raise StackDescriptorInvalidCreate, in_string
    end
  end

  def length
    0
  end

  # PaintMe interface overrides
  def my_colour
    Color.blue
  end


  # End PaintMe interface
end