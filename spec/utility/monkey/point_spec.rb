# Encoding: UTF-8

require 'spec/spec_helper'

describe Point do
  describe 'copy_with_x_and_y_offset' do
    it 'copies as is with nil offsets' do
      p = Point.new(10, 10).copy_with_x_and_y_offset(nil, nil)
      expect(p.x).to eq(10)
      expect(p.y).to eq(10)
    end
    it 'adds the x offset to the base x point' do
      p = Point.new(10, 10).copy_with_x_and_y_offset(5, nil)
      expect(p.x).to eq(15)
      expect(p.y).to eq(10)
    end
    it 'adds the y offset to the base y point' do
      p = Point.new(10, 10).copy_with_x_and_y_offset(nil, 5)
      expect(p.x).to eq(10)
      expect(p.y).to eq(15)
    end
    it 'adds the both  offsets when given' do
      p = Point.new(10, 10).copy_with_x_and_y_offset(5, 8)
      expect(p.x).to eq(15)
      expect(p.y).to eq(18)
    end
  end
end
