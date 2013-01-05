require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'





describe QuantumStackModel do
  describe "instance setup" do
    before(:each) do
      @qs=QuantumStackModel.new
      st=double("StackTranslation", :reverse_lookup => "p", :nil? => false)
      @qs.stack_translation = st
    end
    it "should give an invalid create error when created with incorrect data" do
      expect {
        @qs.quantum_stack = "err"
      }.to raise_error ParserError, /err/
    end
    it "should give an error when stackzero has substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
      }.to raise_error ModelCreateError, /not have/
    end
    it "should give an error when stackvalue has substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Value>0.5</Value></Qstack>"
      }.to raise_error ModelCreateError, /not have/
    end
    it "should give an error when stackqubit does not have substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
      }.to raise_error ModelCreateError, /should have/
    end
    it "should give an error when stackclassical does not have substacks"  do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Classical><cint>14</cint></Classical></Qstack>"
      }.to raise_error ModelCreateError, /should have/
    end
    it "should give an error when stackdata does not have substacks" do
      expect {
        @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"
      }.to raise_error ModelCreateError, /should have/
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
      @qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/><bottom/><bottom/></substacks><Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical></Qstack>"
      @qs.substacks.length.should == 3
    end
  end
  it "should raise an exception if the stack is assigned before the translation" do
    expect {
        qs = QuantumStackModel.new
        qs.quantum_stack = "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
      }.to raise_error ModelCreateError, /Missing/
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