require 'spec/spec_helper'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

describe AbstractDescriptorPainter do
end

describe ClassicalDescriptorPainter do
  before(:each) do
   @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>")
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
   @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>")
  end
  it "should have the colour red" do
    @sd.my_colour.should == Color.red
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
    @sd.get_preferred_size_of_model(g).width.should > 10
    @sd.get_preferred_size_of_model(g).height.should > 15
  end

end

describe ZeroDescriptorPainter do
  before(:each) do
    @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Zero/>")
  end
  it "should have the colour black" do
    @sd.my_colour.should == Color.black
  end
  it "should have a preferred size of W=16, H > 15" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    @sd.get_preferred_size_of_model(g).width.should == 16
    @sd.get_preferred_size_of_model(g).height.should > 15
  end

end
