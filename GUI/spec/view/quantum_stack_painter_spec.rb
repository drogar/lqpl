require 'spec/spec_helper'
require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

      
describe QuantumStackPainter do
  before :each do
    st = double("StackTranslation")
    st.stub(:reverse_lookup, :nil? => false) do |val|
      case val
      when "1" then "@p"
      when "2" then "@q"
      when "3" then "superreallylongnametoforceleftbigger"
      else val
      end
    end
    
    @d=DoDrawing.new
    qh = QuantumStackModel.new
    qh.stack_translation = st
    qh.quantum_stack = QSQBHAD
    @qshad = QuantumStackPainter.new(qh)
    qv = QuantumStackModel.new
    qv.stack_translation = st
    qv.quantum_stack = QSVAL5
    @qsval = QuantumStackPainter.new(qv)
    qb = QuantumStackModel.new
    qb.stack_translation = st
    qb.quantum_stack = "<bottom/>"
    @qsb = QuantumStackPainter.new(qb)

    qi = QuantumStackModel.new
    qi.stack_translation = st
    qi.quantum_stack = QSINT
    @qsint = QuantumStackPainter.new(qi)
    
    qj = QuantumStackModel.new
    qj.stack_translation = st
    qj.quantum_stack = C1WITHBOTTOM
    @qsi_ss_bottom = QuantumStackPainter.new(qj)
    
    qk = QuantumStackModel.new
    qk.stack_translation = st
    qk.quantum_stack = QS3LEVEL
    @qsi_three_ss = QuantumStackPainter.new(qk)
    
    qb = QuantumStackModel.new
    qb.stack_translation = st
    qb.quantum_stack = "<bottom/>"
    @qbottom = QuantumStackPainter.new(qb)
    
  end
  describe "substack_label_placement" do
    it "returns :right for a single substack, index 0" do
      @qsi_ss_bottom.substack_label_placement(0).should == :right
    end
    it "returns :left for ind =0 with 4 substacks" do
      @qshad.substack_label_placement(0).should == :left
    end
    it "returns :left for ind =1 with 4 substacks" do
      @qshad.substack_label_placement(1).should == :left
    end
    it "returns :right for ind =2 with 4 substacks" do
      @qshad.substack_label_placement(2).should == :right
    end
    it "returns :left for ind =3 with 4 substacks" do
      @qshad.substack_label_placement(3).should == :right
    end
    
    it "returns :left for ind =0 with 3 substacks" do
      @qsi_three_ss.substack_label_placement(0).should == :left
    end
    it "returns :right for ind =1 with 3 substacks" do
      @qsi_three_ss.substack_label_placement(1).should == :right
    end
    it "returns :right for ind =2 with 3 substacks" do
      @qsi_three_ss.substack_label_placement(2).should == :right
    end
  end
  describe "substack_label" do
    it "returns the string '27' for index 0 of qsi_three_ss" do
      @qsi_three_ss.substack_label(0).should == "27"
    end
    it "returns the string '5' for index 1 of qsi_three_ss" do
      @qsi_three_ss.substack_label(1).should == "5"
    end
    it "returns the string '7' for index 2 of qsi_three_ss" do
      @qsi_three_ss.substack_label(2).should == "7"
    end
    it "returns 'Nil for model descriptor' for stack==bottom" do
      @qbottom.substack_label(0).should == 'Nil for model descriptor'
    end
  end
  describe "sub_stack_sizes" do
    before :each do
    end
    it "should return an empty array when no substacks" do
      @qsb.sub_stack_sizes(@d.gc).should == []
    end
    it "should return an array of len 4 when there are four substacks" do
      @qshad.sub_stack_sizes(@d.gc).length.should == 4
    end
  end
  describe "paint_substack" do
    it "should put a black line at the top point to the paint point" do
      @qsi_ss_bottom.paint_substack(@d.gc,0,Point.new(10,10),Point.new(10,30))
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_black(rstr,10,11)
      check_raster_point_is_black(rstr,10,20)
      check_raster_point_is_black(rstr,10,29)
    end
  end
  
  describe "paint_substacks" do
    it "should put a black line at the top point to the paint point" do
      @qsi_ss_bottom.paint_substacks(Point.new(10,10),@d.gc)
      rstr = @d.bi.data(Rectangle.new(0,0,30,30))
      check_raster_point_is_black(rstr,10,11)
      check_raster_point_is_black(rstr,10,20)
      check_raster_point_is_black(rstr,10,29)
    end
  end
  describe "paintModel" do
    it "should set some item to black with paintModelAtPoint" do
      @qsval.paintModelAtPoint(@d.gc,Point.new(10,10))
      check_some_point_is_black(@d.bi,0,0,20,20)
    end
    
    it "should set some item to black with paintModel" do
      @qsval.paintModel(@d.gc)
      check_some_point_is_black(@d.bi,0,0,40,40)
    end
  end
  describe "imageOfModel" do
    it "should return an imageicon of the paint" do
      @qsval.imageOfModel.class.should == ImageIcon
    end
  end
  describe "sizing" do
    describe "bottom element size" do
      before (:each) do
        @bottom_size = @qsb.bottom_element_size(@d.gc)
      end
      it "should have a left width of 6" do
        @bottom_size.left_width.should == 6.0
      end
      it "should have a right width of 6" do
        @bottom_size.right_width.should == 6.0
      end
      it "should have a height > 15 and < 16" do
        @bottom_size.height.should > 15
        @bottom_size.height.should < 16
      end
    end
    describe "model paint size" do
      it "should have a preferred size of width > 160 and height > 60 for the hadamard qbit" do
        ps = @qshad.model_paint_size(@d.gc)
        ps.left_width.should > 80.0
        ps.right_width.should > 80.0
        ps.height.should > 60.0
      end
      it "should have a preferred size of width >= 25 and height >= 28 for the value of 0.5 only" do
        ps = @qsval.model_paint_size(@d.gc)
        ps.left_width.should > 12.5
        ps.right_width.should > 12.5
        ps.height.should > 28.0
      end
      it "should have a preferred size > 10, 15 for bottom" do
        ps = @qsb.model_paint_size(@d.gc)
        ps.left_width.should > 5.0
        ps.right_width.should > 5.0
        ps.height.should > 15.0
      end
      it "should have a left size bigger than right width for qsint" do
        @qsint.model_paint_size(@d.gc).left_width > @qsint.model_paint_size(@d.gc).right_width
      end
      it "should have a left size ~= right for the had qubit" do
        (@qsval.model_paint_size(@d.gc).left_width -  @qsval.model_paint_size(@d.gc).right_width).abs.should < 2
      end
    end
  end
end
