# Encoding: UTF-8
require 'spec/spec_helper'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

describe ZeroDescriptorPainter do
  subject do
    DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance '{"zero":0}')
  end
  it 'should have the colour black' do
    expect(subject.my_colour).to eq(Color.black)
  end
  it 'should have a total size of W=55, H > 15' do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    ps = subject.model_paint_size(g)
    expect(ps.left_width).to eq(27.5)
    expect(ps.right_width).to eq(27.5)
    expect(ps.height).to be > 15
  end
  it 'should have a left equal to 1/2 the total width' do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to eq(subject.model_paint_size(g).total_width * 0.5)
  end

end
