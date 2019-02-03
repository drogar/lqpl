
describe Lqpl::Utilities::ArrayPartitioner do
  subject { Lqpl::Utilities::ArrayPartitioner }
  describe :qpl_partion do
    subject { Lqpl::Utilities::ArrayPartitioner.new([1, 2, 3, 4, 5]) }
    it 'should delegate to left partition when called with :left' do
      expect(subject).to receive(:qpl_left_partition)
      subject.qpl_partition(:left)
    end
    it 'should delegate to right partition when called with :right' do
      expect(subject).to receive(:qpl_right_partition)
      subject.qpl_partition(:right)
    end
    it 'should delegate to middle_element when not called with left or right' do
      expect(subject).to receive(:_qpl_middle_element_as_array)
      subject.qpl_partition(:whatever)
    end
  end
  describe 'qpl_left_partition' do
    it 'should get first 2 of a size four array' do
      expect(subject.new([1, 2, 3, 4]).qpl_left_partition).to eq([1, 2])
    end
    it 'should get first 2 of a size five array into 2, 1, 2' do
      expect(subject.new([1, 2, 3, 4, 5]).qpl_left_partition).to eq([1, 2])
    end
    it 'should get first 1 of a size two array' do
      expect(subject.new([1, 2]).qpl_left_partition).to eq([1])
    end
    it 'should get first 1 of a size three array' do
      expect(subject.new([1, 2, 3]).qpl_left_partition).to eq([1])
    end
    it 'should get empty array from a size one array' do
      expect(subject.new([1]).qpl_left_partition).to eq([])
    end
    it 'should get empty array from  a size zero array' do
      expect(subject.new([]).qpl_left_partition).to eq([])
    end
  end

  describe 'get middle element' do
    it 'should get nil for a size 4 array' do
      expect(subject.new([1, 2, 3, 4]).qpl_middle_element).to be_nil
    end
    it 'should get the 3rd element of a size five array' do
      expect(subject.new([1, 2, 3, 4, 5]).qpl_middle_element).to eq(3)
    end
    it 'should get nil for  a size two array into 1, 0, 1' do
      expect(subject.new([1, 2]).qpl_middle_element).to be_nil
    end
    it 'should get the second element of a size three array' do
      expect(subject.new([1, 2, 3]).qpl_middle_element).to eq(2)
    end
    it 'should get the only element of a size one array' do
      expect(subject.new([1]).qpl_middle_element).to eq(1)
    end
    it 'should get nil for  a size zero array' do
      expect(subject.new([]).qpl_middle_element).to be_nil
    end
  end

  describe 'qpl_right_partition' do
    it 'should get the last 2 of a size four array' do
      expect(subject.new([1, 2, 3, 4]).qpl_right_partition).to eq([3, 4])
    end
    it 'should get the last 2 of a size five array' do
      expect(subject.new([1, 2, 3, 4, 5]).qpl_right_partition).to eq([4, 5])
    end
    it 'should get the last  of a size two array' do
      expect(subject.new([1, 2]).qpl_right_partition).to eq([2])
    end
    it 'should get the last  of a size three array' do
      expect(subject.new([1, 2, 3]).qpl_right_partition).to eq([3])
    end
    it 'should get empty from a size one array' do
      expect(subject.new([1]).qpl_right_partition).to eq([])
    end
    it 'should get empty from a size zero array' do
      expect(subject.new([]).qpl_right_partition).to eq([])
    end
  end

  describe 'partition into left mid right' do
    it 'should split a size four array into 2, 0, 2' do
      expect(subject.new([1, 2, 3, 4]).partion_into_left_mid_right).to eq([[1, 2], [], [3, 4]])
    end
    it 'should split a size five array into 2, 1, 2' do
      expect(subject.new([1, 2, 3, 4, 5]).partion_into_left_mid_right).to eq([[1, 2], [3], [4, 5]])
    end
    it 'should split a size two array into 1, 0, 1' do
      expect(subject.new([1, 2]).partion_into_left_mid_right).to eq([[1], [], [2]])
    end
    it 'should split a size three array into 1, 1, 1' do
      expect(subject.new([1, 2, 3]).partion_into_left_mid_right).to eq([[1], [2], [3]])
    end
    it 'should split a size one array into 0, 1, 0' do
      expect(subject.new([1]).partion_into_left_mid_right).to eq([[], [1], []])
    end
    it 'should split a size zero array into 0, 0, 0' do
      expect(subject.new([]).partion_into_left_mid_right).to eq([[], [], []])
    end
  end

  describe 'tails' do
    it 'gives [] for []' do
      expect(subject.new([]).tails).to eq([])
    end

    it 'gives [[1]] for [1]' do
      expect(subject.new([1]).tails).to eq([[1]])
    end

    it 'gives [[1, 2], [2]] for [1, 2]' do
      expect(subject.new([1, 2]).tails).to eq([[1, 2], [2]])
    end

    it 'gives [[1, 2, 3], [2, 3], [3]] for [1, 2, 3]' do
      expect(subject.new([1, 2, 3]).tails).to eq([[1, 2, 3], [2, 3], [3]])
    end
  end

  describe 'heads' do
    it 'gives [] for []' do
      expect(subject.new([]).heads).to eq([])
    end

    it 'gives [[1]] for [1]' do
      expect(subject.new([1]).heads).to eq([[1]])
    end

    it 'gives [[1], [1, 2]] for [1, 2]' do
      expect(subject.new([1, 2]).heads).to eq([[1], [1, 2]])
    end

    it 'gives [[1], [1, 2], [1, 2, 3]] for [1, 2, 3]' do
      expect(subject.new([1, 2, 3]).heads).to eq([[1], [1, 2], [1, 2, 3]])
    end
  end
end
