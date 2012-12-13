


class AbstractDescriptorModel
  include XmlDecode
  attr_accessor :value
  attr_accessor :name

  def self.make_instance(in_string)
    case in_string
    when /^<Zero/ then ZeroDescriptorModel.new in_string
    when /^<Valu/ then ValueDescriptorModel.new in_string
    when /^<Clas/ then ClassicalDescriptorModel.new in_string
    when /^<Qubi/ then QubitDescriptorModel.new in_string
    when /^<Alge/ then DataDescriptorModel.new in_string
    else raise StackDescriptorModelInvalidCreate, in_string
    end
  end

  def initialize
    raise StackDescriptorModelInvalidCreate
  end

  def check_and_return_value(pattern,in_string,data_parser)
    matched = pattern.match in_string
    raise StackDescriptorModelInvalidCreate, "Invalid input for #{self.class}: #{in_string}" if ! matched
    data_parser matched[1]
  end
  
  def length
    return @value.length
  end

  def substack_labels
    nil
  end
end