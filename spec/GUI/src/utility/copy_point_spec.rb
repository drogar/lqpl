require 'copy_point'
describe CopyPoint do
  describe 'copy_with_x_and_y_offset' do
    it 'copies as is with nil offsets' do
      p = CopyPoint.copy_with_x_and_y_offset(Point.new(10, 10), nil, nil)
      expect(p.x).to eq(10)
      expect(p.y).to eq(10)
    end
    it 'adds the x offset to the base x point' do
      p = CopyPoint.copy_with_x_and_y_offset(Point.new(10, 10), 5, nil)
      expect(p.x).to eq(15)
      expect(p.y).to eq(10)
    end
    it 'adds the y offset to the base y point' do
      p = CopyPoint.copy_with_x_and_y_offset(Point.new(10, 10), nil, 5)
      expect(p.x).to eq(10)
      expect(p.y).to eq(15)
    end
    it 'adds the both  offsets when given' do
      p = CopyPoint.copy_with_x_and_y_offset(Point.new(10, 10), 5, 8)
      expect(p.x).to eq(15)
      expect(p.y).to eq(18)
    end
  end
end
