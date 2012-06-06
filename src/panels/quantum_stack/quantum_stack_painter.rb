require "utility/drawing"
require "panels/quantum_stack/descriptor/descriptor_painter_factory"

java_import com.drogar.qface.qstack.Painter
java_import java.awt.Dimension
java_import java.awt.BasicStroke
java_import java.awt.Color
java_import java.awt.Rectangle
java_import java.awt.Point
java_import java.awt.image.BufferedImage
java_import java.awt.geom.Line2D
java_import java.awt.AlphaComposite
java_import javax.swing.ImageIcon

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
    image_size = get_preferred_size_of_model(gstart)
    bifull = BufferedImage.new(image_size.width+20,image_size.height+30,BufferedImage::TYPE_4BYTE_ABGR)
    g = bifull.create_graphics
    paint_model(g,nil)
    ImageIcon.new(bifull)
  end

  alias :image_of_model :imageOfModel

  def paintModel(g, p)
    g.set_rendering_hint(RenderingHints::KEY_ANTIALIASING, RenderingHints::VALUE_ANTIALIAS_ON)

    d = get_preferred_size_of_model(g);
    paint_model_at_point(g,p,Point.new(d.width/2+10.0,20.0))
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

  def getPreferredSizeOfModel(g)
    return @preferred_size if @preferred_size
    return get_string_size(g,"...") if model_element.bottom?
    width = 0.0
    height = 0.0
    @sstack_painters.each do |sstack_painter|
      dimsstack = sstack_painter.get_preferred_size_of_model(g)
      width += [dimsstack.width, node_separation(:horizontal)].max
      height = [height, dimsstack.height + node_separation(:vertical)].max
    end
    dimnode = Dimension.new(0,0)
    dimnode.set_size(@descriptor_painter.get_preferred_size_of_model(g)) if @descriptor_painter
    @preferred_size = Dimension.new([width,dimnode.width].max, [height,dimnode.height].max)
    @preferred_size
  end

  alias :get_preferred_size_of_model :getPreferredSizeOfModel

  # end of painter interface

  def paint_substacks(top_point,g)
    sign_control = (@model_element.substacks.length - 1) * 0.5
    sizes = QuantumStackPainter::compute_offsets(basic_sizes(g))
    @sstack_painters.each_with_index do |sstack_painter,i|

      paint_point = Point.new(top_point.x + (i <=> sign_control)*sizes[i], top_point.y+node_separation(:vertical))

      draw_black_line(g,top_point, paint_point)

      draw_sized_text(g,LINE_LABEL_FONT_SIZE,"#{@model_element.descriptor.substack_labels[i]}",top_point, paint_point,PLACEMENTS[i <=> sign_control])

      sstack_painter.paint_model_at_point(g,p,paint_point)

    end
  end

  def basic_sizes(g)
    @sstack_painters.collect do |sstack_painter|
      [sstack_painter.get_preferred_size_of_model(g).width, node_separation(:horizontal)].max
    end
  end

  def self.compute_offsets(sizes)
    QuantumStackPainter.sum_x_offsets(QuantumStackPainter.make_x_offsets sizes)
  end

  def self.make_x_offsets(subs)
    len = subs.length
    return [0] if len == 1
    if len.even?
      bldr = subs.take(len/2)
      bldr << 0
      bldr += subs.drop(len/2)
    else
      bldr = subs.take(1 + len/2)
      bldr += subs.drop(len/2)
    end
    ret = []
    bldr.each_cons(2) {|pr| ret << (pr[0]+pr[1]) *0.5}
    ret[len/2] = 0 if len.odd?
    ret
  end

  def self.sum_x_offsets(offsets)
    return offsets if offsets.length < 4
    halflen = offsets.length / 2
    ret = []
    offsets.each_with_index do |item,i|
      if i < halflen
        r = offsets[i,halflen-i].inject(0) {|r,el| r+el}
      elsif i >= halflen
        r = offsets[halflen, i+1-halflen].inject(0) {|r,el| r+el}
      else
        r= item
      end
      ret << r
    end
    ret
  end


end