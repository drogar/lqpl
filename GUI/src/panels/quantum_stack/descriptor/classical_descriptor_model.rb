class ClassicalDescriptorModel< AbstractDescriptorModel
  include XmlDecode
  PATTERN=Regexp.new /^<ClassicalStack>(((<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>))*)<\/ClassicalStack>$/

  LIST_PATTERN = Regexp.new /^(<cint>(-?\d+)<\/cint>)|(<cbool>(True|False)<\/cbool>)/

  def initialize(in_string)
    @value = check_and_return_value(PATTERN,in_string,ClassicalDescriptorModel::parse_list)
  end

  def self.parse_list(sub_string)
    r = values_to_list sub_string, LIST_PATTERN do |ret, md|
      ret << md[2].to_i if md[2]
      ret << (md[4] == 'True') if md[4]
    end
    raise StackDescriptorModelInvalidCreate, sub_string if sub_string and sub_string.length > 0 and r.length == 0
    r
  end

  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v}"}
  end

end