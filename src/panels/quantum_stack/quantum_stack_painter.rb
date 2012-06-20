require "utility/drawing"
require "panels/quantum_stack/descriptor/descriptor_painter_factory"

java_import "com.drogar.lqpl.qstack.Painter"


LINE_LABEL_FONT_SIZE = 8.0
PLACEMENTS={-1 => :left, 0 => :right, 1 => :right}

class QuantumStackPainter
  include Painter
  include Drawing

  attr :model_element

  def initialize(model_element)
    @model_element = model_element

    @descriptor_painter = DescriptorPainterFactory.make_painter(model_element.descriptor)
    @sstack_painters = @model_element.substacks.collect {|s| QuantumStackPainter.new(s)}
  end

  def model_element=(model)
    @model_element = model

    @descriptor_painter = DescriptorPainterFactory.make_painter(model.descriptor)
    @sstack_painters = @model_element.substacks.collect {|s| QuantumStackPainter.new(s)}

  end

  # painter interface
  alias :setModelElement :model_element=

  def imageOfModel()
    bistart = BufferedImage.new(10,10,BufferedImage::TYPE_4BYTE_ABGR)
    gstart = bistart.create_graphics
    image_size = model_paint_size(gstart)
    bifull = BufferedImage.new(image_size[:left] + image_size[:right]+40,image_size[:height]+40,BufferedImage::TYPE_4BYTE_ABGR)
    g = bifull.create_graphics
    paint_model(g,nil)
    ImageIcon.new(bifull)
  end

  alias :image_of_model :imageOfModel

  def paintModel(g, p)
    g.set_rendering_hint(RenderingHints::KEY_ANTIALIASING, RenderingHints::VALUE_ANTIALIAS_ON)

    d = model_paint_size(g);
    paint_model_at_point(g,p,Point.new(d[:left]+20.0,20.0))
  end

  alias :paint_model :paintModel

  def paintModelAtPoint(g,p,center)
    if model_element.bottom?
      draw_text_centered_at_point(g,"...", center)
    else
      paint_substacks(center,g)
      @descriptor_painter.paint_model_at_point(g,p,center)

    end
  end

  alias :paint_model_at_point :paintModelAtPoint

  # end of painter interface

  def model_paint_size(g)
    return @preferred_size if @preferred_size
    if model_element.bottom?
      dim= get_string_size(g,"...")
      @preferred_size = {:right => (dim.width * 0.5), :left => (dim.width * 0.5), :height => dim.height}
      return @preferred_size
    end
    left_width = 0.0
    right_width = 0.0
    height = 0.0
    mid_point = (@sstack_painters.length - 1) * 0.5
    @sstack_painters.each_with_index do |sstack_painter,i|
      dimsstack = sstack_painter.model_paint_size(g)
      if i < mid_point
        left_width += [dimsstack[:left] + dimsstack[:right], node_separation(:horizontal)].max
      elsif i > mid_point
        right_width += [dimsstack[:left] + dimsstack[:right], node_separation(:horizontal)].max
      else # i = mid_point
        left_width += [dimsstack[:left], node_separation(:horizontal) *0.5].max
        right_width += [dimsstack[:right], node_separation(:horizontal) *0.5].max
      end
      height = [height, dimsstack[:height] + node_separation(:vertical)].max
    end

    if @descriptor_painter
      dps = @descriptor_painter.model_paint_size(g)
      left_width = [left_width,dps[:left]].max
      right_width = [right_width,dps[:right]].max
      height = [height, dps[:height]].max
    end
    @preferred_size = {:left => left_width, :right => right_width, :height => height}
    @preferred_size
  end


  def paint_substacks(top_point,g)
    sign_control = (@model_element.substacks.length - 1) * 0.5
    sizes = QuantumStackPainter::compute_offsets(basic_sizes(g))
    @sstack_painters.each_with_index do |sstack_painter,i|

      paint_point = Point.new(top_point.x + sizes[i], top_point.y+node_separation(:vertical))

      draw_black_line(g,top_point, paint_point)

      draw_sized_text(g,LINE_LABEL_FONT_SIZE,"#{@model_element.descriptor.substack_labels[i]}",top_point, paint_point,PLACEMENTS[i <=> sign_control])

      sstack_painter.paint_model_at_point(g,p,paint_point)

    end
  end

  def basic_sizes(g)
    ret = []
    @sstack_painters.each do |sstack_painter|
      sstack_size = sstack_painter.model_paint_size(g)
      ret << [sstack_size[:left], node_separation(:horizontal) * 0.5 ].max
      ret << [sstack_size[:right], node_separation(:horizontal) * 0.5].max
    end
    ret
  end

  def self.compute_offsets(sizes)
    return [] if !sizes or sizes.length == 0
    len = sizes.length
    return [0] if len == 2
    add_zero_in_middle = (2 == len %4)
    actives=sizes[1,sizes.length-2]
    mid_point = (len - 2 ) / 2
    bldr = []
    lower_actives = actives.take(mid_point)
    lower_actives.each_with_index do |a,i|
      next if i.odd?
      bldr << - lower_actives[i,mid_point].inject(0) {|sum,la| sum += la}
    end
    bldr << 0 if add_zero_in_middle
    upper_actives = actives.drop(mid_point)
    upper_actives.each_with_index do |a,i|
      next if i.odd? and mid_point.odd?
      next if i.even? and mid_point.even?
      bldr << upper_actives[0,i+1].inject(0) {|sum,la| sum += la}
    end
    bldr
  end


end