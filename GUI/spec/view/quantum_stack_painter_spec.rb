require 'spec/spec_helper'
require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

describe QuantumStackPainter do

  describe "class method compute_offsets" do
    it "should have a single 0 offest for two element lists" do
      QuantumStackPainter::compute_offsets([30,40]).should == [0]
    end
    it "should be [-y,v] for the 4 elt list [_,y,v,_]" do
      QuantumStackPainter::compute_offsets([40,30, 14,50]).should == [-30,14]
    end
    it "should be [-(b+c), 0, (d+e)] for the 6 element list [_,b,c,d,e,_]" do
      QuantumStackPainter::compute_offsets([30, 14, 50,8,10,12]).should == [-64,0,18]
    end
    it "should ignore the first and last element, and then then accumulate the others around the midpoint" do
      QuantumStackPainter::compute_offsets((1..10).to_a).should == [-14,-9,0,13,30]
      QuantumStackPainter::compute_offsets((1..12).to_a).should == [-20,-15,-6,7,24,45]
    end
    it "should return an empty list for empty or nil input" do
      QuantumStackPainter::compute_offsets([]).should == []
      QuantumStackPainter::compute_offsets(nil).should == []
    end
  end
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
    it "should have a default horizontal node sep of 55.0" do
      @qshad.node_separation(:horizontal).should == 55.0
    end

    it "should have a default vertical node sep of 50.0" do
      @qshad.node_separation(:vertical).should == 50.0
    end
    it "should have a preferred size of width > 160 and height > 60 for the hadamard qbit" do
      ps = @qshad.model_paint_size(@g)
      ps[:left].should > 80.0
      ps[:right].should > 80.0
      ps[:height].should > 60.0
    end
    it "should have a preferred size of width >= 25 and height >= 28 for the value of 0.5 only" do
      ps = @qsval.model_paint_size(@g)
      ps[:left].should > 12.5
      ps[:right].should > 12.5
      ps[:height].should > 28.0
    end
    it "should have a preferred size > 10, 15 for bottom" do
      ps = @qsb.model_paint_size(@g)
      ps[:left].should > 5.0
      ps[:right].should > 5.0
      ps[:height].should > 15.0
    end
    it "should have a left size bigger than right width for qsint" do
      @qsint.model_paint_size(@g)[:left] > @qsint.model_paint_size(@g)[:right]
    end
    it "should have a left size ~= right for the had qubit" do
      (@qsval.model_paint_size(@g)[:left] -  @qsval.model_paint_size(@g)[:right]).abs.should < 2
    end
  end
end
