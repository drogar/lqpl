
class StackDescriptor

  def self.make_instance(in_string)
    if in_string =~ /^<Zero/
      StackZero.new in_string
    elsif in_string =~ /^<Value/
      StackValue.new in_string
    elsif in_string =~ /^<Class/
      StackClassical.new in_string
    elsif in_string =~ /^<Qubi/
      StackQubit.new in_string
    elsif in_string =~ /^<Alge/
      StackData.new in_string
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