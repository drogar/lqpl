class StackClassical< StackDescriptor

  PATTERN=Regexp.new /^<ClassicalStack>(((<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>))*)<\/ClassicalStack>$/

  LIST_PATTERN = Regexp.new /^(<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>)/

  def initialize(in_string)
    matc = PATTERN.match in_string
    if matc
      @value = parse_list matc[1]
    else
      raise StackDescriptorInvalidCreate, in_string
    end
  end

  def parse_list(sub_string)
    md = LIST_PATTERN.match(sub_string)
    if !md
      raise StackDescriptorInvalidCreate, sub_string if sub_string and sub_string.length > 0
      []
    elsif md[2]
      [md[2].to_i] + parse_list(sub_string[md[1].length, sub_string.length])
    else
      [md[4] == 'True'] + parse_list(sub_string[md[3].length, sub_string.length])
    end
  end

  def length
    @value.length
  end

end