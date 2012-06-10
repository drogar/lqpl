require 'spec/spec_helper'
require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'
require 'src/panels/quantum_stack/quantum_stack_painter'

describe QuantumStackPainter do
  describe "class method sum_x_offsets" do
    it "should be id one, two and three element lists" do
      QuantumStackPainter::sum_x_offsets([30]).should == [30]
      QuantumStackPainter::sum_x_offsets([30, 14]).should == [30,14]
      QuantumStackPainter::sum_x_offsets([30, 14, 50]).should == [30,14,50]
    end
    it "should take the four elt list [a,b,c,d] tp [a+b, b, c, c+d]" do
      QuantumStackPainter::sum_x_offsets([30, 14, 50, 80]).should == [44,14,50,130]
    end
    it "should have the same number of elements as the input" do
      QuantumStackPainter::sum_x_offsets((1..40).to_a).length.should == 40
    end
    it "should replace elt 'i' with the sum of elts 'i' to the midpoint of the list" do
      QuantumStackPainter::sum_x_offsets([2,4,6,8,10,12]).should == [12,10,6,8,18,30]
      QuantumStackPainter::sum_x_offsets([2,4,0,8,10]).should == [6,4,0,8,18]
    end
  end
  describe "class method make_x_offsets" do
    it "should have a single 0 offest for one element lists" do
      QuantumStackPainter::make_x_offsets([30]).should == [0]
    end
    it "should be 1/2 of each number for two element lists" do
      QuantumStackPainter::make_x_offsets([30, 14]).should == [15,7]
    end
    it "should be 1/2 of sum of the first 2, 0, 1/2 of the sum of the last 2 for 3 element lists" do
      QuantumStackPainter::make_x_offsets([30, 14, 50]).should == [22,0,32]
    end
    it "should be 1/2 of sum of the first 2, 1/2 of second, 1/2 of third, 1/2 of the sum of the last 2 for 4 element lists" do
      QuantumStackPainter::make_x_offsets([30, 14, 50, 80]).should == [22,7,25,65]
    end
    it "should have the same number of elements as the input" do
      QuantumStackPainter::make_x_offsets((1..40).to_a).length.should == 40
    end
    it "should be the 1/2 the sums of adjacent elements for even lists, with the middle two elements being 1/2 of the existing middle" do
      QuantumStackPainter::make_x_offsets([2,4,6,8,10,12]).should == [3,5,3,4,9,11]
    end
    it "should be the 1/2 the sums of adjacent elements for odd lists, with the middle elements being 0" do
      QuantumStackPainter::make_x_offsets([2,4,6,8,10]).should == [3,5,0,7,9]
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
      @g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics

    end
    it "should have a default horizontal node sep of 55.0" do
      @qshad.node_separation(:horizontal).should == 55.0
    end

    it "should have a default vertical node sep of 40.0" do
      @qshad.node_separation(:vertical).should == 40.0
    end
    it "should have a preferred size of width > 160 and height > 60 for the hadamard qbit" do
      ps = @qshad.get_preferred_size_of_model(@g)
      ps.width.should > 160.0
      ps.height.should > 60.0
    end
    it "should have a preferred size of width >= 25 and height >= 28 for the value of 0.5 only" do
      ps = @qsval.get_preferred_size_of_model(@g)
      ps.width.should >= 25
      ps.height.should >= 28
    end
    it "should have a preferred size > 10, 15 for bottom" do
      ps = @qsb.get_preferred_size_of_model(@g)
      ps.width.should > 10.0
      ps.height.should > 15.0
    end
  end
end
