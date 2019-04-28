ImportJava.do_imports(context: self,
                      awt: %w[AlphaComposite BasicStroke Color Dimension Font Point Rectangle RenderingHints
                              font.TextAttribute geom.Dimension2D geom.Line2D geom.Ellipse2D geom.Rectangle2D image.BufferedImage],
                      swing: %w[ImageIcon])

java_import java.text.AttributedString
java_import java.text.AttributedCharacterIterator

module Lqpl
  module Drawing
    # module to supply basic drawing methods to painter classes.
    module DrawMethods
      def draw_text_centered_between(gcontext, text, point1, point2)
        draw_text_centered_at_point(gcontext, text, mid_point(point1, point2))
      end

      def mid_point(point1, point2)
        Point.new(averagex(point1, point2), averagey(point1, point2))
      end

      def get_x_for_reference(reference, width, base_x)
        base_x - width * (reference == :left ? 1 : 0.5)
      end

      def make_attributed_text(text, size)
        atext = AttributedString.new(text)
        atext.add_attribute(TextAttribute::SIZE, size, 0, text.length)
        atext
      end

      def get_text_position(gcontext, reference, atext, point)
        return point if reference == :right

        text_bounds = get_string_size(gcontext, atext.iterator)

        point_with_x_shifted(reference, text_bounds.width, point)
      end

      def point_with_x_shifted(reference, width, point)
        Point.new(get_x_for_reference(reference, width, point.x), point.y)
      end

      def get_string_size(gcontext, text)
        case text
        when AttributedCharacterIterator then
          get_plain_font_string_size(gcontext, text)
        else gcontext.font.get_string_bounds(text, gcontext.font_render_context)
        end
      end

      def get_plain_font_string_size(gcontext, text)
        this_font = gcontext.font.derive_font(Font::PLAIN, 8.0)
        this_font.get_string_bounds(text, 0, text.end_index, gcontext.font_render_context)
      end

      def draw_sized_text(gcontext, size, text, point, reference)
        atext = make_attributed_text(text, size)
        draw_point = get_text_position(gcontext, reference, atext, point)

        draw_text_starting_at_point(gcontext, atext.iterator, draw_point)
      end

      def draw_text_starting_at_point(gcontext, text, point)
        gcontext.draw_string(text, point.x, point.y)
      end

      def draw_text_centered_at_point(gcontext, text, point)
        draw_point = get_text_position(gcontext, :centered, text, point)
        draw_text_starting_at_point(gcontext, text, draw_point)
      end

      def draw_text_to_left_of_point(gcontext, text, point)
        draw_point = get_text_position(gcontext, :left, text, point)
        draw_text_starting_at_point(gcontext, text, draw_point)
      end

      def draw_black_line(gcontext, from, to)
        ln = Line2D::Double.new(from, to)
        gcontext.set_color(Color.black)
        gcontext.draw(ln)
      end

      def draw_colour_filled_shape(gcontext, shape, fill_colour)
        gcontext.set_color(fill_colour)
        gcontext.fill shape
        gcontext.set_color(Color.black)
        gcontext.draw shape
      end

      private

      def averagex(point1, point2)
        (point1.x + point2.x) / 2
      end

      def averagey(point1, point2)
        (point1.y + point2.y) / 2
      end
    end
  end
end
