require 'spec/spec_helper'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

describe AbstractDescriptorPainter do
end

describe ClassicalDescriptorPainter do
  before(:each) do
   @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>")
  end
  after (:all) do
    @sd = nil
  end
  it "should have the colour green" do
    @sd.my_colour.should == Color.green
  end
end

describe DataDescriptorPainter do
  before(:each) do
   @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>")
  end
  it "should have the colour magenta" do
    @sd.my_colour.should == Color.magenta
  end
end

describe QubitDescriptorPainter do
  before(:each) do
    sm=AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sm.name = "Some long name"
    @sd = DescriptorPainterFactory.make_painter(sm)

  end
  it "should have the colour red" do
    @sd.my_colour.should == Color.red
  end

  it "should have a left  width that is more than the right width" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    @sd.model_paint_size(g).left_width.should >  @sd.model_paint_size(g).right_width
  end
end

describe ValueDescriptorPainter do
  before(:each) do
    @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Value>0.5</Value>")
  end
  it "should have the colour blue" do
    @sd.my_colour.should == Color.blue
  end
  it "should have a preferred size of W>10, H > 15" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    @sd.model_paint_size(g).left_width.should > 5
    @sd.model_paint_size(g).right_width.should > 5
    @sd.model_paint_size(g).height.should > 15
  end

  it "should have a left equal to the right" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    @sd.model_paint_size(g).left_width.should ==  @sd.model_paint_size(g).right_width
  end
end

describe ZeroDescriptorPainter do
  before(:each) do
    @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Zero/>")
  end
  it "should have the colour black" do
    @sd.my_colour.should == Color.black
  end
  it "should have a total size of W=55, H > 15" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    ps = @sd.model_paint_size(g)
    ps.left_width.should == 27.5
    ps.right_width.should == 27.5
    ps.height.should > 15
  end
  it "should have a left equal to 1/2 the total width" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    @sd.model_paint_size(g).left_width.should ==  @sd.model_paint_size(g).total_width * 0.5
  end

end
