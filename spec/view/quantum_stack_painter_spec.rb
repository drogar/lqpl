require 'specdata/quantum_stack_data'
require 'quantum_stack_model'
require 'quantum_stack_painter'

describe QuantumStackPainter do
  before :each do
    st = double('StackTranslation')
    allow(st).to receive(:reverse_lookup) do |val|
      case val
      when '1' then '@p'
      when '2' then '@q'
      when '3' then 'superreallylongnametoforceleftbigger'
      else val
      end
    end

    @d = DrawingExtensions.new
    qh = QuantumStackModel.new
    qh.stack_translation = st
    qh.quantum_stack = QSQBHAD
    @qshad = QuantumStackPainter.new(qh)
    qv = QuantumStackModel.new
    qv.stack_translation = st
    qv.quantum_stack = QSVAL5
    @qsval = QuantumStackPainter.new(qv)
    qb = QuantumStackModel.new
    qb.stack_translation = st
    qb.quantum_stack = '{"bottom":true}'
    @qsb = QuantumStackPainter.new(qb)

    qi = QuantumStackModel.new
    qi.stack_translation = st
    qi.quantum_stack = QSINT
    @qsint = QuantumStackPainter.new(qi)

    qj = QuantumStackModel.new
    qj.stack_translation = st
    qj.quantum_stack = C1WITHBOTTOM
    @qsi_ss_bottom = QuantumStackPainter.new(qj)

    qk = QuantumStackModel.new
    qk.stack_translation = st
    qk.quantum_stack = QS3LEVEL
    @qsi_three_ss = QuantumStackPainter.new(qk)

    qb = QuantumStackModel.new
    qb.stack_translation = st
    qb.quantum_stack = '{"bottom":true}'
    @qbottom = QuantumStackPainter.new(qb)
  end

  describe 'substack_label_placement' do
    it 'returns :right for a single substack, index 0' do
      expect(@qsi_ss_bottom.substack_label_placement(0)).to eq(:right)
    end
    it 'returns :left for ind =0 with 4 substacks' do
      expect(@qshad.substack_label_placement(0)).to eq(:left)
    end
    it 'returns :left for ind =1 with 4 substacks' do
      expect(@qshad.substack_label_placement(1)).to eq(:left)
    end
    it 'returns :right for ind =2 with 4 substacks' do
      expect(@qshad.substack_label_placement(2)).to eq(:right)
    end
    it 'returns :left for ind =3 with 4 substacks' do
      expect(@qshad.substack_label_placement(3)).to eq(:right)
    end

    it 'returns :left for ind =0 with 3 substacks' do
      expect(@qsi_three_ss.substack_label_placement(0)).to eq(:left)
    end
    it 'returns :right for ind =1 with 3 substacks' do
      expect(@qsi_three_ss.substack_label_placement(1)).to eq(:right)
    end
    it 'returns :right for ind =2 with 3 substacks' do
      expect(@qsi_three_ss.substack_label_placement(2)).to eq(:right)
    end
  end
  describe 'substack_label' do
    it 'returns the string 27 for index 0 of qsi_three_ss' do
      expect(@qsi_three_ss.substack_label(0)).to eq('27')
    end
    it 'returns the string 5 for index 1 of qsi_three_ss' do
      expect(@qsi_three_ss.substack_label(1)).to eq('5')
    end
    it 'returns the string 7 for index 2 of qsi_three_ss' do
      expect(@qsi_three_ss.substack_label(2)).to eq('7')
    end
    it 'returns Nil for model descriptor for stack==bottom' do
      expect(@qbottom.substack_label(0)).to eq('Nil for model descriptor')
    end
  end
  describe 'sub_stack_sizes' do
    before :each do
    end
    it 'should return an empty array when no substacks' do
      expect(@qsb.sub_stack_sizes(@d.gc)).to eq([])
    end
    it 'should return an array of len 4 when there are four substacks' do
      expect(@qshad.sub_stack_sizes(@d.gc).length).to eq(4)
    end
  end
  describe 'paint_substack' do
    it 'should put a black line at the top point to the paint point' do
      @qsi_ss_bottom.paint_substack(@d.gc, 0, Point.new(10, 10), Point.new(10, 30))
      rstr = @d.bi.data(Rectangle.new(0, 0, 30, 30))
      check_raster_point_is_black(rstr, 10, 11)
      check_raster_point_is_black(rstr, 10, 20)
      check_raster_point_is_black(rstr, 10, 29)
    end
  end

  describe 'paint_substacks' do
    it 'should put a black line at the top point to the paint point' do
      @qsi_ss_bottom.paint_substacks(Point.new(10, 10), @d.gc)
      rstr = @d.bi.data(Rectangle.new(0, 0, 30, 30))
      check_raster_point_is_black(rstr, 10, 11)
      check_raster_point_is_black(rstr, 10, 20)
      check_raster_point_is_black(rstr, 10, 29)
    end
  end
  describe 'paint_model' do
    it 'should set some item to black with paint_model_at_point' do
      @qsval.paint_model_at_point(@d.gc, Point.new(10, 10))
      check_some_point_is_black(@d.bi, 0, 0, 20, 20)
    end

    it 'should set some item to black with paint_model' do
      @qsval.paint_model(@d.gc)
      check_some_point_is_black(@d.bi, 0, 0, 60, 60)
    end
  end
  describe 'imageOfModel' do
    it 'should return an imageicon of the paint' do
      expect(@qsval.image_of_model.class).to eq(ImageIcon)
    end
  end
  describe 'sizing' do
    describe 'bottom element size' do
      before :each do
        @bottom_size = @qsb.bottom_element_size(@d.gc)
      end
      it 'should have a left width of 6' do
        expect(@bottom_size.left_width).to eq(6.0)
      end
      it 'should have a right width of 6' do
        expect(@bottom_size.right_width).to eq(6.0)
      end
      it 'should have a height > 14 and < 16' do
        expect(@bottom_size.height).to be > 14
        expect(@bottom_size.height).to be < 16
      end
    end
    describe 'model paint size' do
      it 'should have a preferred size of width > 160 and height >= 60 for the hadamard qbit' do
        ps = @qshad.model_paint_size(@d.gc)
        expect(ps.left_width).to be > 80.0
        expect(ps.right_width).to be > 80.0
        expect(ps.height).to be >= 60.0
      end
      it 'should have a preferred size of width >= 25 and height >= 28 for the value of 0.5 only' do
        ps = @qsval.model_paint_size(@d.gc)
        expect(ps.left_width).to be > 12.5
        expect(ps.right_width).to be > 12.5
        expect(ps.height).to be > 9 # TODO: is now 9 as of 2016-04-30  - why... was 28.0
      end
      it 'should have a preferred size > 10, 14 for bottom' do
        ps = @qsb.model_paint_size(@d.gc)
        expect(ps.left_width).to be > 5.0
        expect(ps.right_width).to be > 5.0
        expect(ps.height).to be > 14.0
      end
      it 'should have a left size bigger than right width for qsint' do
        @qsint.model_paint_size(@d.gc).left_width > @qsint.model_paint_size(@d.gc).right_width
      end
      it 'should have a left size ~= right for the had qubit' do
        expect((@qsval.model_paint_size(@d.gc).left_width -
          @qsval.model_paint_size(@d.gc).right_width).abs).to be < 2
      end
    end
  end
end
