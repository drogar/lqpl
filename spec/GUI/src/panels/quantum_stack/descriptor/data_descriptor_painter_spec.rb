require 'quantum_stack_model'
require 'quantum_stack_painter'

describe DataDescriptorPainter do
  subject do
    DescriptorPainterFactory
      .make_painter(DescriptorModelFactory
                    .make_model('{"data":[{"cons": "Nil", "addresses": []}]}'))
  end
  it 'should have the colour magenta' do
    expect(subject.my_colour).to eq(Color.magenta)
  end

  it 'should return a Rectangle as its shape' do
    expect(subject.my_shape(Point.new(10, 10)).class).to eq(Rectangle2D::Double)
  end
end
