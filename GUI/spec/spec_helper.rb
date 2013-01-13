require 'rbconfig'
require 'java'

require 'simplecov'
SimpleCov.start do
  add_filter "GUI/spec"
end

# Override at_exit so that rspec actually terminates properly. 
# puts did not seem to work consistently, so using err.println
# result.format! sometimes prints a line as well, but not always.

SimpleCov.at_exit do
  status= $!.is_a?(::SystemExit) ? $!.status : 0
  SimpleCov.result.format!
  java.lang.System.err.println "SimpleCov report generated, covered #{SimpleCov.result.covered_lines} lines of #{SimpleCov.result.total_lines} for a coverage of %#{SimpleCov.result.covered_percent}."
  LqplController.instance.close
  java.lang.System.exit(status)
end

project_dir_array = File.expand_path(File.dirname(__FILE__)).split(File::SEPARATOR)

project_dir = project_dir_array.reverse.drop(2).reverse.join(File::SEPARATOR)
%w{src lqpl_gui lib/java lib/ruby devlib/java}.each do |dir|
  $LOAD_PATH << project_dir+"/GUI/"+ dir
end
$LOAD_PATH << project_dir+"/out/lqpl_gui"

# java classpath
$CLASSPATH << project_dir+"/GUI/lib/java/jruby-complete.jar"
#testing jars
%w{fest-swing-1.2 fest-assert-1.2 fest-reflect-1.2 fest-util-1.1.2 jcip-annotations-1.0}.each do |jar|
  $CLASSPATH << project_dir+"/GUI/devlib/java/" + jar+".jar"
end


require "fest-swing-1.2.jar"

$CLASSPATH << project_dir+"/GUI/lib/java/forms_rt.jar"
$CLASSPATH << project_dir+"/GUI/lib/java/monkeybars-1.1.1.jar"
$CLASSPATH << project_dir+"/out/lqpl_gui"

%w{BasicRobot}.each do |c|
  java_import "org.fest.swing.core."+c
end

%w{GuiActionRunner GuiQuery GuiTask FailOnThreadViolationRepaintManager}.each do |c|
  java_import "org.fest.swing.edt."+c
end

%w{Window}.each do |c|
  java_import "org.fest.swing.finder."+c+"Finder"
end

%w{Component JMenuItem Frame JTextComponent JSpinner JLabel JButton JFileChooser}.each do |c|
  java_import "org.fest.swing.fixture."+c+"Fixture"
end

%w{JButton JLabel Frame Dialog}.each do |c|
  java_import "org.fest.swing.core.matcher."+c+"Matcher"
end
TEST_QP_PATH = project_dir+"/GUI/testdata/qplprograms"

require 'config/platform'
require 'manifest'

class DoDrawing
  include Drawing
  attr_accessor :gc
  attr_accessor :bi
  def initialize
    @bi = BufferedImage.new(500,500,BufferedImage::TYPE_4BYTE_ABGR)
    @gc = @bi.create_graphics
  end
end

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

