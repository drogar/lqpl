# Encoding: UTF-8
require 'spec/spec_helper'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

describe ValueDescriptorPainter do
  subject do
    DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance '{"value":0.5}')
  end
  it 'should have the colour blue' do
    expect(subject.my_colour).to eq(Color.blue)
  end
  it 'should have a preferred size of W>10, H > 15' do
    g = BufferedImage.new(500, 500, BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to be > 5
    expect(subject.model_paint_size(g).right_width).to be > 5
    expect(subject.model_paint_size(g).height).to be > 15
  end

  it 'should have a left equal to the right' do
    g = BufferedImage.new(500, 500, BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to eq(subject.model_paint_size(g).right_width)
  end
end
