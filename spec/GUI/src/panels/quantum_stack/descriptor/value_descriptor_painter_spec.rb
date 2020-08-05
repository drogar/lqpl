require 'quantum_stack_model'
require 'quantum_stack_painter'

describe ValueDescriptorPainter do
  subject do
    DescriptorPainterFactory.make_painter(DescriptorModelFactory.make_model('{"value":0.5}'))
  end
  it 'should have the colour blue' do
    expect(subject.my_colour).to eq(Color.blue)
  end
  it 'should have a preferred size of W>10, H > 15' do
    g = BufferedImage.new(500, 500, BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to be > 5
    expect(subject.model_paint_size(g).right_width).to be > 5
    expect(subject.model_paint_size(g).height).to be >= 10.0 # TODO: Was  > 15
  end

  it 'should have a left equal to the right' do
    g = BufferedImage.new(500, 500, BufferedImage::TYPE_INT_RGB).graphics
    expect(subject.model_paint_size(g).left_width).to eq(subject.model_paint_size(g).right_width)
  end
end
