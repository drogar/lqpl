require 'draw_methods'
require 'descriptor_painter_factory'
require 'copy_point'
require 'canvas_size'

# paint that tree!
class QuantumStackPainter
  include Lqpl::Drawing::DrawMethods
  LINE_LABEL_FONT_SIZE = 8.0
  PLACEMENTS = { -1 => :left, 0 => :right, 1 => :right }.freeze

  attr_reader :model_element

  def initialize(model_element)
    self.model_element = model_element
  end

  def model_element=(model)
    @model_element = model
    @descriptor_painter = DescriptorPainterFactory.make_painter(model.descriptor)
    @sstack_painters = @model_element.substacks.map { |s| QuantumStackPainter.new(s) }
  end

  def image_of_model
    image_buffer = icon_image_buffer(image_size)
    graphic_context = image_buffer.create_graphics

    paint_model(graphic_context)
    ImageIcon.new(image_buffer)
  end

  def paint_model(gcontext)
    gcontext.set_rendering_hint(RenderingHints::KEY_ANTIALIASING,
                                RenderingHints::VALUE_ANTIALIAS_ON)

    d = model_paint_size(gcontext)
    paint_model_at_point(gcontext, Point.new(d.left_required_width + 20.0, 20.0))
  end

  def paint_model_at_point(gcontext, center)
    if model_element.bottom?
      draw_text_centered_at_point(gcontext, '...', center)
    else
      paint_substacks(center, gcontext)
      @descriptor_painter.paint_model_at_point(gcontext, center)
    end
  end

  def bottom_element_size(gcontext)
    dim = get_string_size(gcontext, '...')
    @preferred_size = CanvasSize.new_with_measures(dim.width * 0.5, dim.width * 0.5, dim.height)
  end

  def model_paint_size(gcontext)
    return @preferred_size if @preferred_size
    return bottom_element_size gcontext if model_element.bottom?

    @preferred_size =
      CanvasSize.new_from_subtree(stack_painters_paint_size(gcontext))
    @preferred_size.max_of_each_dimension!(@descriptor_painter.model_paint_size(gcontext)) if @descriptor_painter

    @preferred_size
  end

  def substack_label(index)
    d = @model_element.descriptor.substack_labels if @model_element.descriptor
    return d[index].to_s if d

    'Nil for model descriptor'
    # "#{@model_element.descriptor.substack_labels[index]}"
  end

  def substack_label_placement(index)
    PLACEMENTS[index <=> placement_index]
  end

  def placement_index
    (@model_element.substacks.length - 1) * 0.5
  end

  def paint_substacks(top_point, gcontext)
    offsets = compute_offsets(gcontext)
    paint_range.each do |i|
      paint_at_point =
        CopyPoint.copy_with_x_and_y_offset(top_point,
                                           offsets[i],
                                           CanvasSize.vertical_node_separation)
      paint_substack(gcontext, i, top_point, paint_at_point)
    end
  end

  def paint_substack(gcontext, index, top_point, paint_point)
    draw_black_line(gcontext, top_point, paint_point)
    draw_sized_text(gcontext, LINE_LABEL_FONT_SIZE, substack_label(index),
                    mid_point(top_point, paint_point),
                    substack_label_placement(index))
    @sstack_painters[index].paint_model_at_point(gcontext, paint_point)
  end

  def sub_stack_sizes(gcontext)
    return [] if @sstack_painters.nil?

    @sstack_painters.map { |sstack_painter| sstack_painter.model_paint_size(gcontext) }
  end

  private

  def paint_range
    Range.new(0, @sstack_painters.size - 1)
  end

  def compute_offsets(graphic_context)
    CanvasSize.compute_offsets(sub_stack_sizes(graphic_context))
  end

  def stack_painters_paint_size(gcontext)
    @sstack_painters.map { |element| element.model_paint_size(gcontext) }
  end

  def image_size
    bistart = BufferedImage.new(10, 10, BufferedImage::TYPE_4BYTE_ABGR)
    gstart = bistart.create_graphics
    model_paint_size(gstart)
  end

  def icon_image_buffer(size)
    BufferedImage.new(size.required_width + 40,
                      size.height + 40,
                      BufferedImage::TYPE_4BYTE_ABGR)
  end
end
