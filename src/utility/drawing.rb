java_import java.text.AttributedString
java_import java.text.AttributedCharacterIterator
java_import java.awt.font.TextAttribute

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

end