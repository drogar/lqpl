require 'spec/spec_helper'

QSQBZero = "<Qstack><int>1</int><bool>True</bool>"+
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
QSVAL5 = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"

QSQBHAD = make_multi_sstacks("<Qstack><int>1</int><bool>True</bool>",
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack>",
  QSVAL5,4)

P1 = "<MMap><map><kvpair><key><string>p</string></key><value><int>1</int></value></kvpair></map></MMap>"


describe QuantumStack do
  describe "class method decode_mmap" do
    it "should create the map 1 -> p when input a kv xml with p as key and 1 as val" do
      mm = QuantumStack::decode_mmap(P1)
      mm.should == {1 => "p"}
    end
    it "should create the map 1 -> p, 2 -> rex when input a kv xml with p as key and 1 as val and rex ->2 in same list" do
      mm = QuantumStack::decode_mmap("<MMap><map><kvpair><key><string>p</string></key><value><int>1</int></value></kvpair><kvpair><key><string>rex</string></key><value><int>2</int></value></kvpair></map></MMap>")
      mm.should == {1 => "p", 2 => "rex"}
    end
    it "should create a map with last element of list if duplicated" do
      mm = QuantumStack::decode_mmap("<MMap><map><kvpair><key><string>p</string></key><value><int>27</int></value></kvpair></map><map><kvpair><key><string>rex</string></key><value><int>27</int></value></kvpair></map></MMap>")
      mm.should == {27 => "rex"}
    end
    it "should create a mamp with all entries in the maps in the list" do
      mm = QuantumStack::decode_mmap("<MMap><map><kvpair><key><string>p5</string></key><value><int>27</int></value></kvpair></map><map><kvpair><key><string>p13</string></key><value><int>13</int></value></kvpair><kvpair><key><string>p</string></key><value><int>1</int></value></kvpair></map></MMap>")
      mm.should == {1 => "p", 27 => "p5", 13 => "p13"}
    end
  end
  describe "class method get_next_qstack" do
    it "should return a nil when given the empty string" do
      qsp = QuantumStack::get_next_qstack("")
      qsp.should be_nil
    end
    it "should return a pair of the qstack and empty string when only given only one" do
      qsp = QuantumStack::get_next_qstack(QSVAL5)
      qsp[0].should == QSVAL5
      qsp[1].should == ""
    end
    it "should return a pair of the qstack and empty string when only given only bottom" do
      qsp = QuantumStack::get_next_qstack("<bottom/>")
      qsp[0].should == "<bottom/>"
      qsp[1].should == ""
    end
    it "should return a pair of the qstack and bottom string when only given two bottoms" do
      qsp = QuantumStack::get_next_qstack("<bottom/><bottom/>")
      qsp[0].should == "<bottom/>"
      qsp[1].should == "<bottom/>"
    end
    it "should return the first, then the second when given two and the second call is on the remainder" do
      qsp = QuantumStack::get_next_qstack(QSVAL5+QSVAL5)
      qsp[0].should == QSVAL5
      qsp[1].should == QSVAL5
      qsp2 = QuantumStack::get_next_qstack(qsp[1])
      qsp2[0].should == QSVAL5
      qsp2[1].should == ""
    end

    it "should return the first, then the second when when given two stacks with more substacks" do
      qsp = QuantumStack::get_next_qstack(QSQBHAD+QSQBHAD)
      qsp[0].should == QSQBHAD
      qsp[1].should == QSQBHAD
      qsp2 = QuantumStack::get_next_qstack(qsp[1])
      qsp2[0].should == QSQBHAD
      qsp2[1].should == ""
    end
  end
  describe "class method multiple stacks" do
    it "should create an empty array for no input" do
      rv  = QuantumStack::make_multiple_stacks("")
      rv.length.should == 0
    end
    it "should create a one element array for one" do
      rv = QuantumStack::make_multiple_stacks(""+QSVAL5+"")
      rv.length.should == 1
    end
    it "should create an n element array for n copies" do
      [2,3,4,5,6].each do |i|
        rv = QuantumStack::make_multiple_stacks((""+QSVAL5+"")*i)
        rv.length.should == i
      end
    end
  end
  describe "sizing" do
    before(:each) do
      @qshad = QuantumStack.new(QSQBHAD,P1 )
      @qsval = QuantumStack.new(QSVAL5,"" )
      @qsb = QuantumStack.new("<bottom/>")
      @g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics

    end
    it "should have a default horizontal node sep of 40.0" do
      @qshad.node_separation(:horizontal).should == 40.0
    end

    it "should have a default vertical node sep of 30.0" do
      @qshad.node_separation(:vertical).should == 30.0
    end
    it "should have a preferred size of width > 160 and height > 60 for the hadamard qbit" do
      ps = @qshad.get_preferred_size(@g)
      ps.width.should > 160.0
      ps.height.should > 60.0
    end
    it "should have a preferred size of width  of 10 and height of 31 for the value only" do
      ps = @qsval.get_preferred_size(@g)
      ps.width.should == 10.0
      ps.height.should == 31.0
    end
    it "should have a preferred size > 10, 15 for bottom" do
      ps = @qsb.get_preferred_size(@g)
      ps.width.should > 10.0
      ps.height.should > 15.0
    end

  end
  it "should give an invalid create error when created with incorrect data" do
    expect {
      qs = QuantumStack.new "err"
    }.to raise_error QuantumStackInvalidCreate, /err/
  end
  it "should give an error when stackzero has substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /not have/
  end
  it "should give an error when stackvalue has substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Value>0.5</Value></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /not have/
  end
  it "should give an error when stackqubit does not have substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should give an error when stackclassical does not have substacks"  do
    expect {
      qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><ClassicalStack><cint>14</cint></ClassicalStack></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should give an error when stackdata does not have substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should successfully create the start qstack" do
    qs = QuantumStack.new "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
    qs.should_not be_bottom
  end
  it "should allow 'bottom' as the construction"  do
    qs = QuantumStack.new "<bottom/>"
    qs.should be_bottom
  end
  it "should allow 'bottom' in place of substacks"  do
    qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
  end
  it "should have the same number of substacks as the length of the descriptor" do
    qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    qs.substacks.length.should == 1
    qs = QuantumStack.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/><bottom/><bottom/></substacks><ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack></Qstack>"
    qs.substacks.length.should == 3
  end
  it "should assign a name for a quantum descriptor" do
    qs = QuantumStack.new("<Qstack><int>2</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>",
    "<MMap><map><kvpair><key><string>p</string></key><value><int>2</int></value></kvpair></map></MMap>")
    qs.descriptor.name.should == "p"
  end
end