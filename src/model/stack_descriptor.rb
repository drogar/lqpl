java_import com.drogar.qface.qstack.PaintMe
java_import java.awt.geom.Dimension2D
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.geom.Ellipse2D  #.Double {"Ellipse2Dd"}
java_import java.awt.RenderingHints

class StackDescriptor
  include PaintMe
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

  # PaintMe interface
  def node_size
    10.0
  end

  def node_separation(direction)
    case direction
    when :vertical then 30.0
    when :horizontal then 40.0
    else 20.0
    end
  end

  def paintme(g, p)
    #Not to be used
    raise RuntimeError, "do not call paintme on descriptors, use paintmeAtPoint"

  end

  def get_preferred_size(g)
    width = node_size
    width += g.get_font.get_string_bounds("#{@name}",g.get_font_render_context).width + node_size if @name
    height = node_size
    height += g.get_font.get_string_bounds("#{@value}",g.get_font_render_context).height + (node_size * 0.5) if length == 0
    d = Dimension.new(0,0)
    d.set_size(width, height)
    d
  end

  def paintmeAtPoint(g,p,center)
    half_node_size = node_size * 0.5
    g.set_rendering_hint(RenderingHints::KEY_ANTIALIASING, RenderingHints::VALUE_ANTIALIAS_ON)
    g.set_color(my_colour)
    e = Ellipse2D::Double.new(center.x-half_node_size, center.y-half_node_size, node_size, node_size);
    g.fill(e)
    g.set_color(Color.black);
    g.draw(e);
    draw_name(g,center) if @name
    draw_value(g,center) if length == 0
  end

  def draw_name(g,center)
    text_rec = g.get_font.get_string_bounds("#{@name}",g.get_font_render_context)
    g.draw_string("#{@name}", center.x-text_rec.width - (node_size * 1.5), center.y+(text_rec.height - node_size))
  end

  def draw_value(g, center)
    text_rec = g.get_font.get_string_bounds("#{@value}",g.get_font_render_context)
    g.draw_string("#{@value}", center.x-(text_rec.width * 0.5), center.y+(text_rec.height + node_size))
  end

  def my_colour
    #Not to be used
    raise RuntimeError, "do not call paintme on abstract StackDescriptor, use on subclass: I am a #{self.class}"


  end

  alias :paintme_at_point :paintmeAtPoint
  # End PaintMe interface
end