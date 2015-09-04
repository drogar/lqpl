require 'quantum_stack_model'
require 'quantum_stack_painter'

CDESC = '{"classical":[1,true,14]}'.freeze
describe ClassicalDescriptorElement do
  subject { DescriptorElementFactory.make_element(DescriptorModelFactory.make_model(CDESC)) }

  it 'should have the colour green' do
    expect(subject.my_colour).to eq(Color.green)
  end
  describe 'painting' do
    before :each do
      m = double('model_elelment')
      allow(m).to receive(:name).and_return('Z')
      allow(m).to receive(:value).and_return('Z')
      allow(m).to receive(:length).and_return(0)
      subject = DescriptorElementFactory.make_element(DescriptorModelFactory.make_model(CDESC))
      subject.model_element = m

      @d = DrawingExtensions.new
    end

    it 'should draw the name to the left of the point with paint_model_at_point' do
      # subtracts node size from x rl point is 20,20
      subject.paint_model_at_point(@d.gc, Point.new(30, 20))
      rstr = @d.bi.data(Rectangle.new(0, 0, 30, 30))
      check_some_raster_point_is_grey(rstr, 0, 0, 30, 20)
    end
    it 'should draw the Value centered on the point with paint_value' do
      # adds 2*node size to get y real point is 20,20
      subject.paint_model_at_point(@d.gc, Point.new(20, 0))
      rstr = @d.bi.data(Rectangle.new(0, 0, 30, 30))
      check_some_raster_point_is_grey(rstr, 0, 0, 21, 19)
    end

    it 'should draw a green circle centered on the point with paint_value' do
      # adds node size to y real point is 20,20
      subject.paint_model_at_point(@d.gc, Point.new(20, 20))
      rstr = @d.bi.data(Rectangle.new(0, 0, 30, 30))
      check_raster_point_is_green(rstr, 20, 20)
    end
  end
end
