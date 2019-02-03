%w[AlphaComposite BasicStroke Color Dimension Font Point Rectangle RenderingHints font.TextAttribute image.BufferedImage].each do |name|
  java_import "java.awt.#{name}"
end

%w[Dimension2D Line2D Ellipse2D Rectangle2D].each do |name|
  java_import "java.awt.geom.#{name}"
end

%w[AttributedString AttributedCharacterIterator].each do |name|
  java_import "java.text.#{name}"
end

java_import javax.swing.ImageIcon

module Lqpl
  module Drawing
    # module to supply basic drawing methods to painter classes.
    module DrawMethods
      def draw_text_centered_between(gcontext, text, point1, point2)
        draw_text_centered_at_point(gcontext, text, mid_point(point1, point2))
      end

      def mid_point(point1, point2)
        Point.new((point1.x + point2.x) / 2, (point1.y + point2.y) / 2)
      end

      def draw_sized_text(gcontext, size, text, point, reference)
        atext = AttributedString.new(text)
        atext.add_attribute(TextAttribute::SIZE, size, 0, text.length)
        case reference
        when :left then draw_text_to_left_of_point(gcontext, atext.iterator, Point.new(point.x, point.y))
        when :right then draw_text_to_right_of_point(gcontext, atext.iterator, point)
        else draw_text_centered_at_point(gcontext, atext.iterator, point)
        end
      end

      def draw_text_to_left_of_point(gcontext, text, point)
        text_bounds = get_string_size(gcontext, text)
        gcontext.draw_string(text, point.x - text_bounds.width, point.y)
      end

      def draw_text_to_right_of_point(gcontext, text, point)
        gcontext.draw_string(text, point.x, point.y)
      end

      def draw_text_centered_at_point(gcontext, text, point)
        text_bounds = get_string_size(gcontext, text)
        gcontext.draw_string(text, point.x - (text_bounds.width * 0.5), point.y)
      end

      def get_string_size(gcontext, text)
        case text
        when AttributedCharacterIterator then
          # this_font = gcontext.font.java_send :deriveFont, [Java::float], 8.0
          this_font = gcontext.font.derive_font(Font::PLAIN, 8.0)
          this_font.get_string_bounds(text, 0, text.end_index, gcontext.font_render_context)
        else gcontext.font.get_string_bounds(text, gcontext.font_render_context)
        end
      end

      def draw_black_line(gcontext, from, to)
        ln = Line2D::Double.new(from, to)
        gcontext.set_color(Color.black)
        gcontext.draw(ln)
      end

      def draw_colour_filled_shape(gcontext, shape, fill_colour)
        gcontext.color = fill_colour
        gcontext.fill shape
        gcontext.color = Color.black
        gcontext.draw shape
      end
    end
  end
end
