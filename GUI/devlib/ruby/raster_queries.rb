

def get_sample_data_at_point(rstr,x,y)
  s=[]
  [0,1,2,3].each do |i|
    s<< rstr.getSample(x,y,i)
  end
  s
end

def check_some_point_is_black(img,start_x,start_y,end_x,end_y)
  rstr = img.data(Rectangle.new(start_x,start_y,end_x+1,end_y+1))
      
  xs = Range.new(start_x,end_x)
  ys = Range.new(start_y,end_y)
  (xs.any? do |x|
     ys.any? {|y| get_sample_data_at_point(rstr,x,y)[3] == 255}
   end).should == true
end

def check_raster_point_is_black(rstr,x,y)
  s=get_sample_data_at_point(rstr,x,y)
  
  s[3].should == 255
end

def check_raster_point_is_grey(rstr,x,y)
  s=get_sample_data_at_point(rstr,x,y)
  s[3].should > 50
end

def check_raster_point_is_white(rstr,x,y)
  s=get_sample_data_at_point(rstr,x,y)
  s[3].should == 0
end

def check_raster_point_is_green(rstr,x,y)
  s=get_sample_data_at_point(rstr,x,y)
  
  s[1].should_not == 0
  s[0].should == 0
  s[2].should == 0
  s[3].should_not == 0
end
