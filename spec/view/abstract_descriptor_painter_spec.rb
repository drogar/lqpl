# Encoding: UTF-8
require 'spec/spec_helper'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'
require 'GUI/src/panels/quantum_stack/quantum_stack_painter'

describe AbstractDescriptorPainter do
  subject do
    m = double('model_element')
    allow(m).to receive(:name).and_return('Z')
    allow(m).to receive(:value).and_return('Z')
    allow(m).to receive(:length).and_return(0)
    AbstractDescriptorPainter.new(m)
  end
  describe 'model element' do
    it 'should assign the model element to whatever object is passed in' do
      subject.model_element = 5
      expect(subject.model_element).to eq(5)
    end
  end
  it 'should return an ellipse as its shape' do
    expect(subject.my_shape(Point.new(10, 10)).class).to eq(Ellipse2D::Double)
  end
  it 'should raise a runtime exception for colour' do
    expect do
      subject.my_colour
    end.to raise_error RuntimeError, /abstract/
  end
  it 'should have a nil image of the model' do
    expect(subject.image_of_model).to be_nil
  end
  describe 'drawing items' do
    let(:drawing_extension) { DrawingExtensions.new }
    it 'should draw the name to the left of the point with paint_name' do
      # subtracts node size from x rl point is 20,20
      subject.paint_name(drawing_extension.gc, Point.new(30, 20))
      rstr = drawing_extension.bi.data(Rectangle.new(0, 0, 30, 30))
      #      check_some_raster_point_is_grey(rstr,0,0,29,29)
      check_raster_point_is_grey(rstr, 18, 19)
    end
    it 'should draw the Value centered on the point with paint_value' do
      # adds 2*node size to y real point is 20,20
      subject.paint_value(drawing_extension.gc, Point.new(20, 0))
      rstr = drawing_extension.bi.data(Rectangle.new(0, 0, 30, 30))

      check_raster_point_is_grey(rstr, 19, 19)
      check_raster_point_is_grey(rstr, 21, 19)
    end
    it 'show throw an exception for paint_model_at_point' do
      expect do
        subject.paint_model_at_point(drawing_extension.gc, Point.new(20, 20))
      end.to raise_error RuntimeError, /abstract/
    end
    it 'show throw an exception for paint_model' do
      expect do
        subject.paint_model(drawing_extension.gc)
      end.to raise_error RuntimeError, /paint_model/
    end
  end
end
