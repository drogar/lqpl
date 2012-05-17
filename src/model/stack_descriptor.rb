
class StackDescriptor

  attr_accessor :value

  def self.make_instance(in_string)
    case in_string
    when /^<Zero/ then StackZero.new in_string
    when /^<Valu/ then StackValue.new in_string
    when /^<Clas/ then StackClassical.new in_string
    when /^<Qubi/ then StackQubit.new in_string
    when /^<Alge/ then StackData.new in_string
    else raise StackDescriptorInvalidCreate, in_string
    end
  end

  def initialize
    raise StackDescriptorInvalidCreate
  end

  def length
    return 0
  end

end