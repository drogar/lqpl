require 'utility/drawing'

java_import com.drogar.lqpl.qstack.Painter
java_import java.awt.geom.Dimension2D
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.geom.Ellipse2D  #.Double {"Ellipse2Dd"}
java_import java.awt.geom.Rectangle2D  #.Double {"Ellipse2Dd"}
java_import java.awt.RenderingHints

class AbstractDescriptorPainter
  include Painter
  include Drawing

  def initialize(model)
    @model_element = model
  end

  def model_element=(model)
    @model_element = model
  end

  def model_element
    @model_element
  end

  def node_size
     10.0
  end

  def half_node_size
    node_size * 0.5
  end

  def my_shape(point)
    Ellipse2D::Double.new(point.x-half_node_size, point.y-half_node_size, node_size, node_size)
  end

  def my_colour
    #Not to be used
    raise RuntimeError, "do not call paint on abstract StackDescriptor, use on subclass: I am a #{self.class}"
  end

  # painter interface start
  alias :setModelElement :model_element=

  def imageOfModel()
    nil
  end

  alias :image_of_model :imageOfModel

  def paintModel(g, p)
    #Not to be used
    raise RuntimeError, "do not call paintModel on descriptors, use paintModelAtPoint"
  end

  alias :paint_model :paintModel

  def paintModelAtPoint(g,p,center)
    draw_colour_filled_shape(g,my_shape(center), my_colour)
    draw_text_to_left_of_point(g,"#{@model_element.name}",Point.new(center.x-node_size, center.y)) if @model_element.name
    draw_text_centered_at_point(g,"#{@model_element.value}",Point.new(center.x, center.y+node_size)) if @model_element.length == 0
  end

  alias :paint_model_at_point :paintModelAtPoint
  # end of painter interface

  def model_paint_size(g)

    left_width = half_node_size
    left_width += get_string_size(g,"#{@model_element.name}").width + node_size if @model_element.name
    right_width = half_node_size

    height  =  node_size
    valsize =  get_string_size(g," #{@model_element.value} ")
    height  += valsize.height + half_node_size if @model_element.length == 0
    right_width   =  [right_width,valsize.width*0.5].max
    left_width = [left_width,valsize.width*0.5].max
    {:left => left_width, :right => right_width, :height => height}
  end



end