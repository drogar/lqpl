require 'right_origin_text'

describe Lqpl::Drawing::RightOriginText do
  let(:draw_ext) { DrawingExtensions.new }
  subject { Lqpl::Drawing::RightOriginText.new(graphics_context: draw_ext.gc) }

  describe 'get_string_size' do
    it 'returns a size for a regular string "ab" w=15,  h>14' do
      s = subject.get_string_size('ab')
      expect(s.width).to eq(15.0)
      expect(s.height).to be > 14.0
    end
    it 'returns a size for an attributed string "ab" of w=9,  h>9' do
      ab = java.text.AttributedString.new('ab')
      ab.add_attribute(java.text.AttributedCharacterIterator::Attribute::LANGUAGE,
                       java.util.Locale.new('en'))
      s = subject.get_string_size(ab.iterator)
      expect(s.width).to eq(9.0)
      expect(s.height).to be > 9.0
    end
  end

  describe 'draw_text_at_point' do
    it 'should darken a pixel just to the right of the point' do
      subject.draw_text_at_point('N', Point.new(50, 50))

      rstr = draw_ext.bi.data(Rectangle.new(0, 0, 60, 60))
      check_raster_point_is_grey(rstr, 46, 46)
    end
  end

  describe 'draw_sized_text_at_point' do
    it 'should darken a pixel to the left of the midpoint with :left' do
      subject.draw_sized_text_at_point(10, 'Z', draw_ext.mid_point(Point.new(40, 50), Point.new(60, 50)))

      rstr = draw_ext.bi.data(Rectangle.new(0, 0, 70, 70))

      check_raster_point_is_grey(rstr, 49, 49)
    end
  end
end
