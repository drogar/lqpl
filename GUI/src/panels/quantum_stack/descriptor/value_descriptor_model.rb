

class ValueDescriptorModel < AbstractDescriptorModel

  PATTERN=Regexp.new /^<Value>(<number>)?(.*?)(<\/number>)?<\/Value>$/

  def initialize(in_string)
    matc = PATTERN.match in_string
    raise StackDescriptorInvalidCreate, in_string if ! matc
    @value = matc[2]
    @name = nil
  end

  def length
    0
  end

end