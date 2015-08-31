# encoding: utf-8

LINE_LABEL_FONT_SIZE = 8.0
PLACEMENTS = { -1 => :left, 0 => :right, 1 => :right }
# paint that tree!
class QuantumStackPainter
  include Drawing

  attr_accessor :model_element

  def initialize(model_element)
    self.model_element = model_element
  end

  def model_element=(model)
    @model_element = model
    @descriptor_painter =
      DescriptorPainterFactory.make_painter(model.descriptor)
    @sstack_painters = @model_element.substacks.map { |s| QuantumStackPainter.new(s) }
  end

  def image_of_model
    bistart = BufferedImage.new(10, 10, BufferedImage::TYPE_4BYTE_ABGR)
    gstart = bistart.create_graphics
    image_size = model_paint_size(gstart)
    bifull = BufferedImage.new(image_size.required_width + 40,
                               image_size.height + 40,
                               BufferedImage::TYPE_4BYTE_ABGR)
    g = bifull.create_graphics
    paint_model(g)
    ImageIcon.new(bifull)
  end

  def paint_model(g)
    g.set_rendering_hint(RenderingHints::KEY_ANTIALIASING,
                         RenderingHints::VALUE_ANTIALIAS_ON)

    d = model_paint_size(g)
    paint_model_at_point(g, Point.new(d.left_required_width + 20.0, 20.0))
  end

  def paint_model_at_point(g, center)
    if model_element.bottom?
      draw_text_centered_at_point(g, '...', center)
    else
      paint_substacks(center, g)
      @descriptor_painter.paint_model_at_point(g, center)
    end
  end

  def bottom_element_size(g)
    dim = get_string_size(g, '...')
    @preferred_size = CanvasSize.new_with_measures(dim.width * 0.5, dim.width * 0.5, dim.height)
  end

  def model_paint_size(g)
    return @preferred_size if @preferred_size
    return bottom_element_size g if model_element.bottom?

    @preferred_size =
      CanvasSize.new_from_subtree(
        @sstack_painters.map { |painter| painter.model_paint_size(g) })

    @preferred_size.max_of_each_dimension!(
      @descriptor_painter.model_paint_size(g)) if @descriptor_painter

    @preferred_size
  end

  def substack_label(index)
    d = @model_element.descriptor.substack_labels if @model_element.descriptor
    return d[index].to_s if d
    'Nil for model descriptor'
    # "#{@model_element.descriptor.substack_labels[index]}"
  end

  def substack_label_placement(index)
    PLACEMENTS[index <=> (@model_element.substacks.length - 1) * 0.5]
  end

  def paint_substacks(top_point, g)
    offsets = CanvasSize.compute_offsets(sub_stack_sizes(g))
    Range.new(0, @sstack_painters.size - 1).each do |i|
      paint_at_point =
        CopyPoint.copy_with_x_and_y_offset(top_point,
                                           offsets[i],
                                           CanvasSize.vertical_node_separation)
      paint_substack(g, i, top_point, paint_at_point)
    end
  end

  def paint_substack(g, index, top_point, paint_point)
    draw_black_line(g, top_point, paint_point)
    draw_sized_text(g, LINE_LABEL_FONT_SIZE, substack_label(index),
                    mid_point(top_point, paint_point),
                    substack_label_placement(index))
    @sstack_painters[index].paint_model_at_point(g, paint_point)
  end

  def sub_stack_sizes(g)
    @sstack_painters.map { |sstack_painter| sstack_painter.model_paint_size(g) }
  end
end
