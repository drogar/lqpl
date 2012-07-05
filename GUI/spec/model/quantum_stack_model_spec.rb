require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'





describe QuantumStackModel do


  describe "class method get_next_qstack" do
    it "should return a nil when given the empty string" do
      qsp = QuantumStackModel::get_next_qstack("")
      qsp.should be_nil
    end
    it "should return a pair of the qstack and empty string when only given only one" do
      qsp = QuantumStackModel::get_next_qstack(QSVAL5)
      qsp[0].should == QSVAL5
      qsp[1].should == ""
    end
    it "should return a pair of the qstack and empty string when only given only bottom" do
      qsp = QuantumStackModel::get_next_qstack("<bottom/>")
      qsp[0].should == "<bottom/>"
      qsp[1].should == ""
    end
    it "should return a pair of the qstack and bottom string when only given two bottoms" do
      qsp = QuantumStackModel::get_next_qstack("<bottom/><bottom/>")
      qsp[0].should == "<bottom/>"
      qsp[1].should == "<bottom/>"
    end
    it "should return the first, then the second when given two and the second call is on the remainder" do
      qsp = QuantumStackModel::get_next_qstack(QSVAL5+QSVAL5)
      qsp[0].should == QSVAL5
      qsp[1].should == QSVAL5
      qsp2 = QuantumStackModel::get_next_qstack(qsp[1])
      qsp2[0].should == QSVAL5
      qsp2[1].should == ""
    end

    it "should return the first, then the second when when given two stacks with more substacks" do
      qsp = QuantumStackModel::get_next_qstack(QSQBHAD+QSQBHAD)
      qsp[0].should == QSQBHAD
      qsp[1].should == QSQBHAD
      qsp2 = QuantumStackModel::get_next_qstack(qsp[1])
      qsp2[0].should == QSQBHAD
      qsp2[1].should == ""
    end
  end
  describe "class method multiple stacks" do
    before(:each) do
      @st=double("StackTranslation", :reverse_lookup => "p", :nil? => false)
    end
    it "should create an empty array for no input" do
      rv  = QuantumStackModel::make_multiple_stacks("",@st)
      rv.length.should == 0
    end
    it "should create a one element array for one" do
      rv = QuantumStackModel::make_multiple_stacks(""+QSVAL5+"",@st)
      rv.length.should == 1
    end
    it "should create an n element array for n copies" do
      [2,3,4,5,6].each do |i|
        rv = QuantumStackModel::make_multiple_stacks((""+QSVAL5+"")*i,@st)
        rv.length.should == i
      end
    end
  end
  describe "instance setup" do
    before(:each) do
      @qs=QuantumStackModel.new
      st=double("StackTranslation", :reverse_lookup => "p", :nil? => false)
      @qs.stack_translation = st
    end
    it "should give an invalid create error when created with incorrect data" do
      expect {
        @qs.quantum_stack = "err"
      }.to raise_error QuantumStackModelInvalidCreate, /err/
    end
    it "should give an error when stackzero has substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /not have/
    end
    it "should give an error when stackvalue has substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Value>0.5</Value></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /not have/
    end
    it "should give an error when stackqubit does not have substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /should have/
    end
    it "should give an error when stackclassical does not have substacks"  do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><ClassicalStack><cint>14</cint></ClassicalStack></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /should have/
    end
    it "should give an error when stackdata does not have substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /should have/
    end
    it "should successfully create the start qstack" do
      @qs.quantum_stack = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
      @qs.should_not be_bottom
    end
    describe "instance method make_name" do
      before(:each) do
        st = double("StackTranslation", :nil? => false)
        st.stub(:reverse_lookup) do |val|
          case val
          when 1 then "@q"
          when 2 then "@r"
          when -1 then "-1"
          else "#{@val}"
          end
        end
        @qs.stack_translation=st
      end
      it "returns the stackaddress as a string if not found in the translation" do
        @qs.quantum_stack = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
        @qs.make_name(:use_stack_address).should == ""
        @qs.make_name(:hide_stack_address).should == ""
      end
      it "returns the name(stackaddress) as a string if stackaddress found in the translation with use_stack_address symbol passed" do
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
        @qs.make_name(:use_stack_address).should == "@q(1)"
      end
      it "returns the name as a string if stackaddress found in the translation with hide_stack_address symbol passed" do
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
        @qs.make_name(:hide_stack_address).should == "@q"
      end
    end
    it "should allow 'bottom' as the construction"  do
      @qs.quantum_stack = "<bottom/>"
      @qs.should be_bottom
    end
    it "should allow 'bottom' in place of substacks"  do
      @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    end
    it "should have the same number of substacks as the length of the descriptor" do
      @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
      @qs.substacks.length.should == 1
      @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/><bottom/><bottom/></substacks><ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack></Qstack>"
      @qs.substacks.length.should == 3
    end
  end
  it "should raise an exception if the stack is assigned before the translation" do
    expect {
        qs = QuantumStackModel.new
        qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
      }.to raise_error QuantumStackModelInvalidCreate, /Missing/
  end
  it "should assign a name for a quantum descriptor" do
    qs = QuantumStackModel.new
    st = double("StackTranslation", :nil? => false)
    st.should_receive(:reverse_lookup).and_return("p")
    qs.stack_translation=st
    qs.quantum_stack =(QB2WITHBOTTOM)
    qs.descriptor.name.should == "p(2)"
  end
  it "should assign names to multi-level qstacks" do
    qs = QuantumStackModel.new
    st = double("StackTranslation", :nil? => false)
    st.stub(:reverse_lookup) do |val|
      case val
      when 1 then "@q"
      when 2 then "@r"
      when -1 then "-1"
      end
    end
    qs.stack_translation=st
    qs.quantum_stack =(QSQ1R2)
    qs.descriptor.name.should == "@r(2)"
    qs.substacks[0].descriptor.name.should == "@q(1)"
  end
end