module Lqpl
  module Drawing
    # text drawing methods - draw to right of point
    class RightOriginText
      attr_reader :graphics_context
      def initialize(graphics_context:)
        @graphics_context = graphics_context
      end

      def draw_sized_text_at_point(size, text, point)
        atext = AttributedString.new(text)
        atext.add_attribute(TextAttribute::SIZE, size, 0, text.length)
        draw_text_at_point(atext.iterator, point)
      end

      def draw_text_at_point(text, point)
        text_bounds = get_string_size(text)
        graphics_context.draw_string(text, point.x - (text_bounds.width * 0.5), point.y)
      end

      def get_string_size(text)
        case text
        when AttributedCharacterIterator then
          # this_font = g.font.java_send :deriveFont, [Java::float], 8.0
          this_font = graphics_context.font.derive_font(Font::PLAIN, 8.0)
          this_font.get_string_bounds(text, 0, text.end_index, graphics_context.font_render_context)
        else graphics_context.font.get_string_bounds(text, graphics_context.font_render_context)
        end
      end
    end
  end
end
