require 'draw_methods'

describe Lqpl::Drawing::DrawMethods do
  let(:the_font) do
    sub_font = double('java-font8', get_string_bounds: Rectangle.new(18, 8))
    double('java-font', get_string_bounds: Rectangle.new(30, 15), derive_font: sub_font)
  end
  let(:graphic_context) do
    double('java-gc',
           font: the_font,
           font_render_context: 0,
           color: Color.black,
           draw: nil,
           draw_string: nil,
           fill: nil)
  end

  subject do
    de = DrawingExtensions.new
    de.gc = graphic_context
    de
  end

  describe 'mid_point' do
    it 'returns (10, 10) for input points (0, 0) and 20, 20' do
      expect(subject.mid_point(Point.new(0, 0), Point.new(20, 20))).to eq(Point.new(10, 10))
    end
  end

  describe 'get_x_for_reference' do
    it 'returns x - width if reference is :left' do
      expect(subject.get_x_for_reference(:left, 17, 50)).to eq 33
    end
    it 'returns x - 1/2 width if reference is not :left' do
      expect(subject.get_x_for_reference(:center, 17, 50)).to eq 41.5
      expect(subject.get_x_for_reference(:right, 14, 50)).to eq 43
      expect(subject.get_x_for_reference(:garbage, 14, 50)).to eq 43
    end
  end

  describe :make_attributed_text do
    it 'adds a size attribute of the text lengths' do
      a_text = double('atext')
      allow(AttributedString).to receive(:new).and_return(a_text)
      allow(a_text).to receive(:add_attribute)

      subject.make_attributed_text('some text', 15)

      expect(AttributedString).to have_received(:new).with('some text')
      expect(a_text).to have_received(:add_attribute).with(TextAttribute::SIZE, 15, 0, 9) # 'some_text' length is 9
    end
  end

  describe 'get_text_postion' do
    # text width is 30 in all cases due to test double above
    let(:atext) { double('t', iterator: 'tt') }
    it 'returns the same point when reference is :right' do
      p = Point.new(100, 100)
      tp = subject.get_text_position(subject.gc, :right, atext, p)
      expect(tp.x).to eq(p.x)
      expect(tp.y).to eq(p.y)
    end
    it 'returns a point to the left by the width of the string when reference is :left' do
      p = Point.new(100, 100)
      tp = subject.get_text_position(subject.gc, :left, atext, p)
      expect(tp.x).to eq(p.x - 30)
      expect(tp.y).to eq(p.y)
    end
    it 'returns a point to the left by half the width of the string when reference is not :left or :right' do
      p = Point.new(100, 100)
      tp = subject.get_text_position(subject.gc, :center, atext, p)
      expect(tp.x).to eq(p.x - 15)
      expect(tp.y).to eq(p.y)
    end
  end

  describe 'get_string_size' do
    it 'returns the contexts base font size when called with text' do
      s = subject.get_string_size(subject.gc, 'ab')
      expect(s.width).to eq(30)
      expect(s.height).to eq(15)
    end
    it 'returns the contexts derived size for an attributed string' do
      ab = java.text.AttributedString.new('ab')
      ab.add_attribute(java.text.AttributedCharacterIterator::Attribute::LANGUAGE,
                       java.util.Locale.new('en'))
      s = subject.get_string_size(subject.gc, ab.iterator)
      expect(s.width).to eq(18)
      expect(s.height).to eq(8)
    end
  end

  describe :get_plain_font_string_size do
    it 'returns the derived font text size' do
      d_font = double('dfont')
      t = double('t', end_index: 1)
      allow(subject.gc).to receive(:font_render_context).and_return(:context)
      allow(d_font).to receive(:get_string_bounds).and_return(:success)
      allow(subject.gc.font).to receive(:derive_font).and_return(d_font)

      expect(subject.get_plain_font_string_size(subject.gc, t)).to eq :success

      expect(subject.gc.font).to have_received(:derive_font).with(Font::PLAIN, 8.0)
      expect(d_font).to have_received(:get_string_bounds).with(t, 0, 1, :context)
    end
  end

  describe 'draw_sized_text' do
    it 'delegates to other functions' do
      a_text = double('atext', iterator: :itr)
      allow(subject).to receive(:make_attributed_text).and_return(a_text)
      allow(subject).to receive(:get_text_position).and_return(:draw_point)
      allow(subject).to receive(:draw_text_starting_at_point).and_return(:done)

      p = Point.new(40, 50)
      result = subject.draw_sized_text(subject.gc, 10, 'Z', p, :centered)

      expect(result).to eq :done

      expect(subject).to have_received(:make_attributed_text).with('Z', 10)
      expect(subject).to have_received(:get_text_position).with(subject.gc, :centered, a_text, p)
      expect(subject).to(have_received(:draw_text_starting_at_point)
                           .with(subject.gc, :itr, :draw_point))
    end
  end

  describe 'draw_text_starting_at_point' do
    it 'calls draw_string on the context with the text, and the points x,y values' do
      allow(subject.gc).to receive(:draw_string)

      subject.draw_text_starting_at_point(subject.gc, 'N', Point.new(10, 17))

      expect(subject.gc).to have_received(:draw_string).with('N', 10, 17)
    end
  end

  describe 'draw_black_line' do
    it 'should set the color and draw a line' do
      allow(subject.gc).to receive(:set_color)
      allow(subject.gc).to receive(:draw)
      subject.draw_black_line(subject.gc, Point.new(1, 1), Point.new(1, 10))
      expect(subject.gc).to have_received(:set_color).with(Color.black)
      expect(subject.gc).to have_received(:draw).with(kind_of(Line2D::Double))
    end
  end

  describe :draw_colour_filled_shape do
    it 'should set the color and draw the shape' do
      allow(subject.gc).to receive(:set_color)
      allow(subject.gc).to receive(:fill)
      allow(subject.gc).to receive(:draw)

      subject.draw_colour_filled_shape(subject.gc, :shape, :colour)

      expect(subject.gc).to have_received(:set_color).with(:colour)
      expect(subject.gc).to have_received(:fill).with(:shape)
      expect(subject.gc).to have_received(:set_color).with(Color.black)
      expect(subject.gc).to have_received(:draw).with(:shape)
    end
  end
end
