# Abstract painter base
class AbstractDescriptorPainter
  include Lqpl::Drawing::DrawMethods

  attr_accessor :model_element
  def initialize(model)
    @model_element = model
  end

  def node_size
    10.0
  end

  def half_node_size
    node_size * 0.5
  end

  def my_shape(point)
    Ellipse2D::Double.new(point.x - half_node_size,
                          point.y - half_node_size,
                          node_size,
                          node_size)
  end

  def my_colour
    # Not to be used
    raise "do not call paint on abstract StackDescriptor, use on subclass: I am a #{self.class}"
  end

  def image_of_model
    nil
  end

  def paint_model(_unused)
    # Not to be used
    raise 'do not call paint_model on descriptors, use paint_model_at_point'
  end

  def paint_model_at_point(gcontext, center)
    draw_colour_filled_shape(gcontext, my_shape(center), my_colour)
    paint_name(gcontext, center) if @model_element.name
    paint_value(gcontext, center) if @model_element.scalar?
  end

  def paint_name(gcontext, center)
    draw_text_to_left_of_point(gcontext, @model_element.name.to_s,
                               Point.new(center.x - node_size, center.y))
  end

  def paint_value(gcontext, center)
    draw_text_centered_at_point(gcontext, @model_element.value.to_s,
                                Point.new(center.x, center.y + 2 * node_size))
  end

  def model_paint_size(gcontext)
    height  =  node_size
    valsize =  get_value_canvas_size gcontext
    height += valsize.height_with_spacing if @model_element.scalar?

    right_width   =  valsize.right_required_width
    left_width    =  [half_node_size + name_width(gcontext),
                      valsize.left_required_width].max

    CanvasSize.new_with_measures(left_width, right_width, height)
  end

  def get_value_canvas_size(gcontext)
    valsize = get_string_size(gcontext, " #{@model_element.value} ")
    CanvasSize.new_with_measures(valsize.width * 0.5, valsize.width * 0.5, valsize.height)
  end

  def name_width(gcontext)
    return get_string_size(gcontext, @model_element.name.to_s).width + node_size if @model_element.name

    0
  end
end
