require 'spec/spec_helper'
require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

describe QuantumStackPainter do


  describe "get_preferred_size_of_model" do

    it "should cache the size after the first call"
    it "should check if the model element is bottom and return "
    it "should request the size of all substacks on the first call when not bottom"
    it "should request the size of the descriptor on the first call when not bottom"

  end
  
  describe "sizing" do
    before(:each) do
      st = double("StackTranslation")
      st.stub(:reverse_lookup, :nil? => false) do |val|
        case val
        when "1" then "@p"
        when "2" then "@q"
        when "3" then "superreallylongnametoforceleftbigger"
        else val
        end
      end
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
      @g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics

    end
    describe "bottom element size" do
      before (:each) do
        @bottom_size = @qsb.bottom_element_size(@g)
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
        ps = @qshad.model_paint_size(@g)
        ps.left_width.should > 80.0
        ps.right_width.should > 80.0
        ps.height.should > 60.0
      end
      it "should have a preferred size of width >= 25 and height >= 28 for the value of 0.5 only" do
        ps = @qsval.model_paint_size(@g)
        ps.left_width.should > 12.5
        ps.right_width.should > 12.5
        ps.height.should > 28.0
      end
      it "should have a preferred size > 10, 15 for bottom" do
        ps = @qsb.model_paint_size(@g)
        ps.left_width.should > 5.0
        ps.right_width.should > 5.0
        ps.height.should > 15.0
      end
      it "should have a left size bigger than right width for qsint" do
        @qsint.model_paint_size(@g).left_width > @qsint.model_paint_size(@g).right_width
      end
      it "should have a left size ~= right for the had qubit" do
        (@qsval.model_paint_size(@g).left_width -  @qsval.model_paint_size(@g).right_width).abs.should < 2
      end
    end
  end
end
