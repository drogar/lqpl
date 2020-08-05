require 'canvas_size'
require 'size_computer'

describe SizeComputer do
  let(:e1) { CanvasSize.new_with_measures(211, 303, 2) }
  let(:e2) { CanvasSize.new_with_measures(213, 305, 23) }
  let(:e3) { CanvasSize.new_with_measures(217, 307, 29) }
  subject { SizeComputer.new([e1, e2, e3]) }
  describe 'height' do
    it 'returns the max of height with spacing calls' do
      expect(subject.height).to eq 79 # 29 plus 50 for node separation
    end
  end
  describe 'left_width' do
    it 'adds total width of the left element to the left width of the mid' do
      expect(subject.left_width).to eq 727
    end
  end
  describe 'right_width' do
    it 'adds total width of the right element to the right width of the mid' do
      expect(subject.right_width).to eq 829
    end
  end

  describe 'total_width' do
    it 'adds the left width and the right width' do
      expect(subject.total_width).to eq 1556
    end
  end

  describe 'compute_offsets' do
    it 'should have a single 0 offest for a single measure' do
      c1 = CanvasSize.new_with_measures(10, 99, 0)
      sut = SizeComputer.new([c1])
      expect(sut.compute_offsets).to eq([0])
    end
    it 'should be [-right,left] for a 2 elt list' do
      c1 = CanvasSize.new_with_measures(10, 99, 0)
      c2 = CanvasSize.new_with_measures(75, 85, 0)

      sut = SizeComputer.new([c1, c2])
      expect(sut.compute_offsets).to eq([-99, 75])
    end
    it 'should be [-(1.r+2.l), 0, (2.r+3.l)] for 3 element' do
      c1 = CanvasSize.new_with_measures(10, 99, 0)
      c2 = CanvasSize.new_with_measures(75, 85, 0)
      c3 = CanvasSize.new_with_measures(100, 100, 0)

      sut = SizeComputer.new([c1, c2, c3])
      expect(sut.compute_offsets).to eq([-174, 0, 185])
    end
    it 'should be [-(1.r+2.t), -2.r, 3.l, (3.t+4.l)] for 4 element' do
      c1 = CanvasSize.new_with_measures(10, 99, 0)
      c2 = CanvasSize.new_with_measures(75, 85, 0)
      c3 = CanvasSize.new_with_measures(100, 100, 0)
      c4 = CanvasSize.new_with_measures(200, 200, 0)

      sut = SizeComputer.new([c1, c2, c3, c4])
      expect(sut.compute_offsets).to eq([-(99 + 75 + 85), -85, 100, 200 + 200])
    end
    it 'should be [-(1.r+2.t+3.l), -(2.r+3.l),0, (3.r), 3.t+4.l, 3.t+4.t+5.l] for 5 element' do
      c1 = CanvasSize.new_with_measures(10, 99, 0)
      c2 = CanvasSize.new_with_measures(75, 85, 0)
      c3 = CanvasSize.new_with_measures(80, 120, 0)
      c4 = CanvasSize.new_with_measures(200, 200, 0)
      c5 = CanvasSize.new_with_measures(60, 60, 0)

      sut = SizeComputer.new([c1, c2, c3, c4, c5])
      expect(sut.compute_offsets).to eq([-(99 + 75 + 85 + 80), -(85 + 80), 0, 120 + 200, 120 + 200 + 200 + 60])
    end
    it 'should return an empty list for empty or nil input' do
      expect(SizeComputer.new([]).compute_offsets).to eq([])
      expect(SizeComputer.new(nil).compute_offsets).to eq([])
    end
    describe 'left_offsets' do
      it 'should be [-99] for a 2 elt list' do
        c1 = CanvasSize.new_with_measures(10, 99, 0)
        c2 = CanvasSize.new_with_measures(75, 85, 0)

        sut = SizeComputer.new([c1, c2])
        expect(sut.left_offsets).to eq([-99])
      end
    end
    describe 'right_offsets' do
      it 'should be [75] for a 2 elt list' do
        c1 = CanvasSize.new_with_measures(10, 99, 0)
        c2 = CanvasSize.new_with_measures(75, 85, 0)

        sut = SizeComputer.new([c1, c2])
        expect(sut.right_offsets).to eq([75])
      end
    end
  end
end
