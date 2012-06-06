class ClassicalDescriptorModel< AbstractDescriptorModel

  PATTERN=Regexp.new /^<ClassicalStack>(((<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>))*)<\/ClassicalStack>$/

  LIST_PATTERN = Regexp.new /^(<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>)/

  def initialize(in_string)
    matc = PATTERN.match in_string
    raise StackDescriptorModelInvalidCreate, in_string if ! matc
    @value = ClassicalDescriptorModel::parse_list matc[1]
  end

  def self.parse_list(sub_string)
    md = LIST_PATTERN.match(sub_string)
    raise StackDescriptorModelInvalidCreate, sub_string if !md and sub_string and sub_string.length > 0
    ret = []
    return ret if !md
    len = md[0].length
    while md
      ret << md[2].to_i if md[2]
      ret << (md[4] == 'True') if md[4]
      md = LIST_PATTERN.match(sub_string[len, sub_string.length])
      return ret if !md
      len += md[0].length
    end
  end

  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v}"}
  end

end