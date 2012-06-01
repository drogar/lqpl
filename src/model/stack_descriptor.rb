require 'utility/drawing'

java_import com.drogar.qface.qstack.PaintMe
java_import java.awt.geom.Dimension2D
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.geom.Ellipse2D  #.Double {"Ellipse2Dd"}
java_import java.awt.geom.Rectangle2D  #.Double {"Ellipse2Dd"}
java_import java.awt.RenderingHints



class StackDescriptor
  include PaintMe
  include Drawing
  attr_accessor :value
  attr_accessor :name

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
    return @value.length
  end

  def substack_labels
    nil
  end

  # PaintMe interface

  def imageOfMe()
    return nil
  end

  def node_size
    10.0
  end
  def half_node_size
    node_size * 0.5
  end



  def paintme(g, p)
    #Not to be used
    raise RuntimeError, "do not call paintme on descriptors, use paintmeAtPoint"

  end

  def my_shape(point)
    Ellipse2D::Double.new(point.x-half_node_size, point.y-half_node_size, node_size, node_size)
  end

  def get_preferred_size(g)
    width   =  node_size
    width   += get_string_size(g,"#{@name}").width + node_size if @name
    height  =  node_size
    valsize =  get_string_size(g," #{@value} ")
    height  += valsize.height + (node_size * 0.5) if length == 0
    width   =  [width,valsize.width].max
    d = Dimension.new(0,0)
    d.set_size(width, height)
    d
  end

  def paintmeAtPoint(g,p,center)
    g.set_rendering_hint(RenderingHints::KEY_ANTIALIASING, RenderingHints::VALUE_ANTIALIAS_ON)
    g.set_color(my_colour)
    e = my_shape(center);
    g.fill(e)
    g.set_color(Color.black);
    g.draw(e);
    draw_text_to_left_of_point(g,"#{@name}",Point.new(center.x-node_size, center.y-node_size)) if @name
    draw_text_centered_at_point(g,"#{@value}",Point.new(center.x, center.y+node_size)) if length == 0
  end



  def my_colour
    #Not to be used
    raise RuntimeError, "do not call paintme on abstract StackDescriptor, use on subclass: I am a #{self.class}"


  end

  alias :paintme_at_point :paintmeAtPoint
  alias :getPreferredSize :get_preferred_size
  # End PaintMe interface
end