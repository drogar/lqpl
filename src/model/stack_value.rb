class StackValue < StackDescriptor

  attr_accessor :value

  PATTERN=Regexp.new /^<Value>((0?\.\d*)|(1\.0*))<\/Value>$/

  def initialize(in_string)
    matc = PATTERN.match in_string
    if matc
      @value = matc[1].to_f
    else
      raise StackDescriptorInvalidCreate, in_string
    end
  end


end