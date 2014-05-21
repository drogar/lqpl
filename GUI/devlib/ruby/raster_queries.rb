# encoding: UTF-8

def get_sample_data_at_point(rstr, x, y)
  s = []
  [0, 1, 2, 3].each do |i|
    s << rstr.getSample(x, y, i)
  end
  s
end

def check_some_point_is_black(img, start_x, start_y, end_x, end_y)
  rstr = img.data(Rectangle.new(start_x, start_y, end_x + 1, end_y + 1))

  xs = Range.new(start_x, end_x)
  ys = Range.new(start_y, end_y)
  expect(xs.any? { |x|  ys.any? { |y| get_sample_data_at_point(rstr, x, y)[3] == 255 } }).to be true
end

# Not currently used
# def check_some_point_is_grey(img,start_x,start_y,end_x,end_y)
#  rstr = img.data(Rectangle.new(start_x,start_y,end_x+1,end_y+1))
#  check_some_raster_point_is_grey(rstr,start_x,start_y,end_x,end_y)
# end

def check_some_raster_point_is_grey(rstr, start_x, start_y, end_x, end_y)
  xs = Range.new(start_x, end_x)
  ys = Range.new(start_y, end_y)
  #  xs.each {|x| ys.each {|y| puts "x=#{x}  y=#{y},  sd=#{get_sample_data_at_point(rstr,x,y)}" }}
  expect(xs.any? { |x| ys.any? { |y| get_sample_data_at_point(rstr, x, y)[3] > 50 } }).to be true
end

def check_raster_point_is_black(rstr, x, y)
  s = get_sample_data_at_point(rstr, x, y)

  expect(s[3]).to eql(255)
end

def check_raster_point_is_grey(rstr, x, y)
  s = get_sample_data_at_point(rstr, x, y)

  expect(s[3]).to be > 50
end

def check_raster_point_is_white(rstr, x, y)
  s = get_sample_data_at_point(rstr, x, y)
  expect(s[3]).to eql(0)
end

def check_raster_point_is_green(rstr, x, y)
  s = get_sample_data_at_point(rstr, x, y)

  expect(s[1]).to_not eql(0)
  expect(s[0]).to eql(0)
  expect(s[2]).to eql(0)
  expect(s[3]).to_not eql(0)
end
