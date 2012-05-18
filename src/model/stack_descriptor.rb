java_import com.drogar.qface.qstack.PaintMe
java_import java.awt.Dimension
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle

class StackDescriptor
  include PaintMe
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

  # PaintMe interface

  def paintme(g, p)
    #Not to be used
    raise RuntimeException, "do not call paintme on descriptors, use paintmeAtPoint"

  end

  def paintmeAtPoint(g,p,center)
    #Not to be used
    raise RuntimeException, "do not call paintme on abstract StackDescriptor, use on subclass"


  end

  alias :paintme_at_point :paintmeAtPoint
  # End PaintMe interface
end