# Encoding: UTF-8
require 'spec/spec_helper'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

describe DataDescriptorPainter do
  subject do
    DescriptorPainterFactory
      .make_painter(AbstractDescriptorModel
                    .make_instance '{"data":[{"cons": "Nil", "addresses": []}]}')
  end
  it 'should have the colour magenta' do
    expect(subject.my_colour).to eq(Color.magenta)
  end

  it 'should return a Rectangle as its shape' do
    expect(subject.my_shape(Point.new(10, 10)).class).to eq(Rectangle2D::Double)
  end
end
