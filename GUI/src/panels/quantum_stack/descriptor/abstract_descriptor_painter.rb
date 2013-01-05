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

  def paintModel(g)
    #Not to be used
    raise RuntimeError, "do not call paintModel on descriptors, use paintModelAtPoint"
  end

  alias :paint_model :paintModel

  def paintModelAtPoint(g,center)
    draw_colour_filled_shape(g,my_shape(center), my_colour)
    paint_name(g,center) if @model_element.name
    paint_value(g,center) if @model_element.length == 0
  end

  def paint_name(g,center)
    draw_text_to_left_of_point(g,"#{@model_element.name}",Point.new(center.x-node_size, center.y))
  end
  
  def paint_value(g,center)
    draw_text_centered_at_point(g,"#{@model_element.value}",Point.new(center.x, center.y+node_size)) 
  end
  
  alias :paint_model_at_point :paintModelAtPoint
  # end of painter interface

  def model_paint_size(g)

    height  =  node_size
    valsize =  get_value_canvas_size g
    height  += valsize.height_with_spacing if @model_element.length == 0
    
    right_width   =  valsize.right_required_width
    left_width    =  [half_node_size + name_width(g),valsize.left_required_width].max
    
    CanvasSize.new_with_measures(left_width, right_width, height)
  end

  def get_value_canvas_size(g)
    valsize = get_string_size(g," #{@model_element.value} ")
    CanvasSize.new(valsize.width*0.5,valsize.width*0.5,valsize.height)
  end
  
  def name_width g
    return get_string_size(g,"#{@model_element.name}").width + node_size if @model_element.name
    0
  end


end