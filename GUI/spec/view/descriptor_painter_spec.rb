require 'spec/spec_helper'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'




describe AbstractDescriptorPainter do
  before(:each) do
    m=double("model_elelment")
    m.stub(:name).and_return("Z")
    m.stub(:value).and_return("Z")
    m.stub(:length).and_return(0)
    @adp=AbstractDescriptorPainter.new(m)
  end
  describe "model element" do
    it "should assign the model element to whatever object is passed in" do
      @adp.model_element= 5
      @adp.model_element.should == 5
    end
  end
  it "should return an ellipse as its shape" do
    @adp.my_shape(Point.new(10,10)).class.should == Ellipse2D::Double
  end
  it "should raise a runtime exception for colour" do
    expect {
      @adp.my_colour
    }.to raise_error RuntimeError,/abstract/
  end
  it "should have a nil image of the model" do
    @adp.image_of_model.should be_nil
  end
  describe "drawing items" do
    before :each do
      @d=DrawingExtensions.new
    end
    it "should draw the name to the left of the point with paint_name" do
      @adp.paint_name(@d.gc,Point.new(30,20)) # subtracts node size from x rl point is 20,20
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      #      check_some_raster_point_is_grey(rstr,0,0,29,29)
      check_raster_point_is_grey(rstr,18,19)
    end
    it "should draw the Value centered on the point with paint_value" do
      @adp.paint_value(@d.gc,Point.new(20,0)) # adds 2*node size to y real point is 20,20
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
            
      check_raster_point_is_grey(rstr,19,19)
      check_raster_point_is_grey(rstr,21,19)
    end
    it "show throw an exception for paint_model_at_point" do
      expect {
        @adp.paint_model_at_point(@d.gc,Point.new(20,20))
      }.to raise_error RuntimeError,/abstract/
    end
    it "show throw an exception for paint_model" do
      expect {
        @adp.paint_model(@d.gc)
      }.to raise_error RuntimeError,/paint_model/
    end
  end
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
  describe "painting" do
    before (:each) do
      m=double("model_elelment")
      m.stub(:name).and_return("Z")
      m.stub(:value).and_return("Z")
      m.stub(:length).and_return(0)
      @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>")
      @sd.model_element= m
      
      @d=DrawingExtensions.new
    end
    
    it "should draw the name to the left of the point with paint_model_at_point" do
      @sd.paint_model_at_point(@d.gc,Point.new(30,20)) # subtracts node size from x rl point is 20,20
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_grey(rstr,18,19)
    end
    it "should draw the Value centered on the point with paint_value" do
      @sd.paint_model_at_point(@d.gc,Point.new(20,0)) # adds 2*node size to get y real point is 20,20
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_grey(rstr,19,19)
      check_raster_point_is_grey(rstr,21,19)
    end
    
    it "should draw a green circle centered on the point with paint_value" do
      @sd.paint_model_at_point(@d.gc,Point.new(20,20)) # adds node size to y real point is 20,20
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_green(rstr,20,20)
    end
  end
end

describe DataDescriptorPainter do
  before(:each) do
   @sd = DescriptorPainterFactory.make_painter(AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>")
  end
  it "should have the colour magenta" do
    @sd.my_colour.should == Color.magenta
  end
  
  it "should return a Rectangle as its shape" do
    @sd.my_shape(Point.new(10,10)).class.should == Rectangle2D::Double
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
