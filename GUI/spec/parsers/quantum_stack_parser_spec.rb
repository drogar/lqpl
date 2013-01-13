require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'


describe QuantumStackParser do
  describe "class method" do
    describe "initial_qstack_regexp" do
      it "should equal the embeddable with a ^ at the start" do
        QuantumStackParser.initial_qstack_regexp.source.should == "^" + QuantumStackParser.embeddable_top_level_regexp.source
      end
    end
  end
  describe "multiple substack layers" do
    it "should correctly parse an item with no substacks" do
      q = QuantumStackParser.new QSVAL5
      q.substacks.length.should == 0
    end
    
    it "should correctly parse an item with one layer of substacks" do
      q = QuantumStackParser.new QSINT
      q.substacks.length.should == 2
      q.substacks[0].substacks.length.should == 0
    end
    
    it "should correctly parse an item with multi layers of substacks" do
      q = QuantumStackParser.new QS3LEVEL
      q.substacks.length.should == 3
      q.substacks[2].substacks.length.should == 1
      q.substacks[2].substacks[0].substacks.length.should == 0
    end
  end
  
  describe "bottom stacks" do
    it "should return a bottom=true for '<bottom/>'" do
      q = QuantumStackParser.new "<bottom/>"
      q.bottom?.should be_true
    end
    BOTTOMS.each do |b|
      it "parses  stack with bottom" do
        q = QuantumStackParser.new b
        q.parsed?.should be_true
      end
      it "returns a single 'bottom' qs for the substacks of these" do
        q = QuantumStackParser.new b
        q.substacks[0].bottom?.should be_true
      end
    end
  end
  describe "stackaddress" do
    ADDRESSES.each do |m|
      it "should return #{m[0]} for the stack address" do
        q = QuantumStackParser.new m[1]
        q.stackaddress.should == m[0]
      end
    end
  end
  describe "diagonal" do
    DIAGS.each do |d|
      it "should return #{d[0]} for the on_diagonal" do
        q=QuantumStackParser.new d[1]
        q.on_diagonal?.should == d[0]
      end
    end
  end
  describe "descriptors" do
    it "should return '<Zero/> for a zero descriptor" do
      q = QuantumStackParser.new QSZ
      q.descriptor.should == "<Zero/>"
    end
    it "should return '<Value>... for a Value descriptor" do
      q = QuantumStackParser.new QSVAL5
      q.descriptor.should == "<Value><number>0.5</number></Value>"
    end
    it "should return '<Alg...' for a data descriptor" do
      q = QuantumStackParser.new AL1WITHBOTTOM
      q.descriptor.should == "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    end
    it "should return '<Class...' for a classical descriptor" do
      q = QuantumStackParser.new C1WITHBOTTOM
      q.descriptor.should == "<Classical><cint>27</cint></Classical>"
    end
    it "should return '<Qub...' for a qubit descriptor" do
      q = QuantumStackParser.new QSQBZero
      q.descriptor.should == "<Qubits><pair><qz/><qz/></pair></Qubits>"
    end
  end
end
    
  