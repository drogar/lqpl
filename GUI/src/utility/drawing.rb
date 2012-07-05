["AlphaComposite","BasicStroke","Color","Dimension","Point","Rectangle","RenderingHints"].each do |name|
  java_import "java.awt.#{name}"
end

java_import java.awt.font.TextAttribute

["Dimension2D","Line2D","Ellipse2D","Rectangle2D"].each do |name|
  java_import "java.awt.geom.#{name}"
end

java_import java.awt.image.BufferedImage

["AttributedString","AttributedCharacterIterator"].each do |name|
  java_import "java.text.#{name}"
end

java_import javax.swing.ImageIcon

VERTICAL_NODE_SEPARATION=50.0
HORIZONTAL_NODE_SEPARATION=55.0

module Drawing
  def draw_text_centered_at_point(g,text, point)
      text_bounds = get_string_size(g,text)
      g.draw_string(text, point.x-(text_bounds.width*0.5), point.y+(text_bounds.height * 0.5))
  end

  def draw_text_centered_between(g,text, point1, point2)
      point = Point.new((point1.x + point2.x) / 2, (point1.y + point2.y) / 2)
      draw_centered_text(g,text,point)
  end

  def draw_sized_text(g,size,text, point1, point2,reference)
    point = Point.new((point1.x + point2.x) / 2, (point1.y + point2.y) / 2)
    atext = AttributedString.new(text)
    atext.add_attribute(TextAttribute::SIZE, size, 0, text.length)
    case reference
    when :left then draw_text_to_left_of_point(g,atext.iterator,Point.new(point.x,point.y))
    when :right then draw_text_to_right_of_point(g,atext.iterator,point)
    else draw_text_centered_between(g,atext.iterator,point1,point2)
    end
  end

  def draw_text_to_left_of_point(g,text, point)
    text_bounds = get_string_size(g,text)
    g.draw_string(text, point.x-text_bounds.width, point.y)
  end

  def draw_text_to_right_of_point(g,text, point)
    g.draw_string(text, point.x, point.y)
  end

  def get_string_size(g,text)
    case text
    when AttributedCharacterIterator then
      this_font = g.font.derive_font(8.0)
      this_font.get_string_bounds(text,0, text.end_index, g.font_render_context)
    else g.font.get_string_bounds(text,g.font_render_context)
    end
  end

  def draw_black_line(g,from,to)
    ln = Line2D::Double.new(from,to)
    g.set_color(Color.black)
    g.draw(ln)
  end

  def draw_colour_filled_shape(g,shape,fill_colour)
    g.color = fill_colour
    g.fill shape
    g.color = Color.black
    g.draw shape
  end


  def node_separation(direction)
    case direction
    when :vertical then VERTICAL_NODE_SEPARATION
    when :horizontal then HORIZONTAL_NODE_SEPARATION
    else VERTICAL_NODE_SEPARATION
    end
  end




end