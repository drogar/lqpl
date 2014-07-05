# Encoding: UTF-8
require 'spec/spec_helper'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

QZZ = '{"qubit": ["ZZ"]}'
describe QubitDescriptorPainter do
  subject do
    sm = AbstractDescriptorModel.make_instance QZZ
    sm.name = 'Some long name'
    DescriptorPainterFactory.make_painter(sm)
  end
  it 'should have the colour red' do
    expect(subject.my_colour).to eq(Color.red)
  end

  it 'should have a left  width that is more than the right width' do
    g = BufferedImage.new(500, 500, BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to be >  subject.model_paint_size(g).right_width
  end
end
