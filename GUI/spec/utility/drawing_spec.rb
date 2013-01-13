require 'spec/spec_helper'

class DoDrawing
  include Drawing
  attr_accessor :gc
  attr_accessor :bi
  def initialize
    @bi = BufferedImage.new(500,500,BufferedImage::TYPE_4BYTE_ABGR)
    @gc = @bi.create_graphics
  end
end



describe Drawing do
  before :each do
    @d = DoDrawing.new
  end
  describe "mid_point" do
    it "returns (10,10) for points (0,0) and 20,20" do
      @d.mid_point(Point.new(0,0),Point.new(20,20)).should == Point.new(10,10)
    end
  end
  describe "get_string_size" do
    it "returns a size for a regular string 'ab' w=15, h>15" do
      s=@d.get_string_size(@d.gc,"ab")
      s.width.should == 15.0
      s.height.should > 15.0
    end
    it "returns a size for an attributed string of w=9, h>9" do
      ab = java.text.AttributedString.new("ab");
      ab.add_attribute(java.text.AttributedCharacterIterator::Attribute::LANGUAGE, 
          java.util.Locale.new("en"))
      s=@d.get_string_size(@d.gc,ab.iterator)
      s.width.should == 9.0
      s.height.should > 9.0  
    end  
  end
  describe "draw_black_line" do
    it "should fill in pixels on a line when drawing from 1,1 to 1,10 and nowhere else" do
      @d.draw_black_line(@d.gc, Point.new(1,1), Point.new(1,10))
      rstr = @d.bi.data(Rectangle.new(0,0,6,11))
      check_raster_point_is_black(rstr,1,3)
      check_raster_point_is_black(rstr,1,9)
      check_raster_point_is_white(rstr,2,2)
    end
  end
  describe "draw_text_to_left_of_point" do
    it "should darken a pixel just to the left of the point" do
      @d.draw_text_to_left_of_point(@d.gc,"N",Point.new(50,50))
      
      rstr = @d.bi.data(Rectangle.new(0,0,60,60))
      check_raster_point_is_grey(rstr,48,48)
    end
  end
  
  describe "draw_text_to_right_of_point" do
    it "should darken a pixel just to the right of the point" do
      @d.draw_text_to_right_of_point(@d.gc,"N",Point.new(50,50))
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,51,48)
    end
  end
  describe "draw_text_centered_point" do
    it "should darken a pixel to the left and the right of the point" do
      @d.draw_text_centered_at_point(@d.gc,"Z",Point.new(50,50))
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,51,49)
      check_raster_point_is_grey(rstr,49,49)
    end
  end
  
  describe "draw_text_centered_between" do
    it "should darken a pixel to the left and the right of the midpoint" do
      @d.draw_text_centered_between(@d.gc,"Z",Point.new(40,50),Point.new(60,50))
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,51,49)
      check_raster_point_is_grey(rstr,49,49)
    end
  end
  describe "draw_sized_text" do
    it "should darken a pixel to the left and the right of the midpoint with :centered" do
      @d.draw_sized_text(@d.gc,10,"Z",Point.new(40,50),Point.new(60,50),:centered)
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,51,49)
      check_raster_point_is_grey(rstr,49,49)
    end
    it "should darken a pixel to the right of the midpoint with :right" do
      @d.draw_sized_text(@d.gc,10,"Z",Point.new(40,50),Point.new(60,50),:centered)
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,51,49)
    end
    it "should darken a pixel to the left  of the midpoint with :left" do
      @d.draw_sized_text(@d.gc,10,"Z",Point.new(40,50),Point.new(60,50),:left)
      
      rstr = @d.bi.data(Rectangle.new(0,0,70,70))
      
      check_raster_point_is_grey(rstr,49,49)
    end
  end
  describe "draw_colour_filled_shape" do
    it "should fill with the requested colour" do
      @d.draw_colour_filled_shape(@d.gc,Ellipse2D::Double.new(20, 20,10,10), Color.green)
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_green(rstr,25,25)
    end
    it "should edge with black" do
      @d.draw_colour_filled_shape(@d.gc,Ellipse2D::Double.new(20, 20,10,10), Color.green)
      rstr = @d.bi.data(Rectangle.new(0,0,40,40))
      check_raster_point_is_black(rstr,25,20)
    end
  end
end