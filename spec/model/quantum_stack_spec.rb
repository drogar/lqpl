require 'spec/spec_helper'

QSQBZero = "<Qstack><int>1</int><bool>True</bool>"+
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
QSVAL5 = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"

QSQBHAD = make_multi_sstacks("<Qstack><int>1</int><bool>True</bool>",
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack>",
  QSVAL5,4)

KVP1 = "<kvpair><key><string>p</string></key><value><int>1</int></value></kvpair>"
KVP1REX2 = KVP1 +
    "<kvpair><key><string>rex</string></key><value><int>2</int></value></kvpair>"

KVP27 = "<kvpair><key><string>p</string></key><value><int>27</int></value></kvpair>"
KVREX27 ="<kvpair><key><string>rex</string></key><value><int>27</int></value></kvpair>"
KVTH13 = "<kvpair><key><string>th</string></key><value><int>13</int></value></kvpair>"

P1 = "<MMap><map>"+KVP1+"</map></MMap>"
P1ANDEMPTY = "<MMap><map></map><map>"+KVP1+"</map></MMap>"




describe QuantumStack do
  describe "class method sum_x_offsets" do
    it "should be id one, two and three element lists" do
      QuantumStack::sum_x_offsets([30]).should == [30]
      QuantumStack::sum_x_offsets([30, 14]).should == [30,14]
      QuantumStack::sum_x_offsets([30, 14, 50]).should == [30,14,50]
    end
    it "should take the four elt list [a,b,c,d] tp [a+b, b, c, c+d]" do
      QuantumStack::sum_x_offsets([30, 14, 50, 80]).should == [44,14,50,130]
    end
    it "should have the same number of elements as the input" do
      QuantumStack::sum_x_offsets((1..40).to_a).length.should == 40
    end
    it "should replace elt 'i' with the sum of elts 'i' to the midpoint of the list" do
      QuantumStack::sum_x_offsets([2,4,6,8,10,12]).should == [12,10,6,8,18,30]
      QuantumStack::sum_x_offsets([2,4,0,8,10]).should == [6,4,0,8,18]
    end
  end
  describe "class method make_x_offsets" do
    it "should have a single 0 offest for one element lists" do
      QuantumStack::make_x_offsets([30]).should == [0]
    end
    it "should be 1/2 of each number for two element lists" do
      QuantumStack::make_x_offsets([30, 14]).should == [15,7]
    end
    it "should be 1/2 of sum of the first 2, 0, 1/2 of the sum of the last 2 for 3 element lists" do
      QuantumStack::make_x_offsets([30, 14, 50]).should == [22,0,32]
    end
    it "should be 1/2 of sum of the first 2, 1/2 of second, 1/2 of third, 1/2 of the sum of the last 2 for 4 element lists" do
      QuantumStack::make_x_offsets([30, 14, 50, 80]).should == [22,7,25,65]
    end
    it "should have the same number of elements as the input" do
      QuantumStack::make_x_offsets((1..40).to_a).length.should == 40
    end
    it "should be the 1/2 the sums of adjacent elements for even lists, with the middle two elements being 1/2 of the existing middle" do
      QuantumStack::make_x_offsets([2,4,6,8,10,12]).should == [3,5,3,4,9,11]
    end
    it "should be the 1/2 the sums of adjacent elements for odd lists, with the middle elements being 0" do
      QuantumStack::make_x_offsets([2,4,6,8,10]).should == [3,5,0,7,9]
    end
  end
  describe "class method kv_pairs_to_map" do
    it "should return the empty map if given nil" do
      QuantumStack::kv_pairs_to_map(nil).should be_empty
    end
    it "should return the empty map if there are no pairs" do
      QuantumStack::kv_pairs_to_map("").should be_empty
    end
    it "should return a value -> key map of a single entry" do
      QuantumStack::kv_pairs_to_map(KVP1).should == {1 => "p"}
    end
    it "should return a value -> key map with the last entry winning" do
      QuantumStack::kv_pairs_to_map(KVP27+KVREX27).should == {27 => "rex"}
    end

    it "should return a value -> key map with the all entries if no dup values" do
      QuantumStack::kv_pairs_to_map(KVP1+KVREX27+KVTH13).should == {1 => "p", 27 => "rex", 13 => "th"}
    end
  end
  describe "class method decode_mmap" do
    it "should create the map 1 -> p when input a kv xml with p as key and 1 as val" do
      mm = QuantumStack::decode_mmap(P1)
      mm.should == {1 => "p"}
    end
     it "should create the map 1 -> p when input a kv xml with p as key and 1 as val when there is an empty map in front" do
        mm = QuantumStack::decode_mmap(P1ANDEMPTY)
        mm.should == {1 => "p"}
      end
    it "should create the map 1 -> p, 2 -> rex when input a kv xml with p as key and 1 as val and rex ->2 in same list" do
      mm = QuantumStack::decode_mmap("<MMap><map>"+KVP1REX2+"</map></MMap>")
      mm.should == {1 => "p", 2 => "rex"}
    end
    it "should create a map with last element of list if duplicated" do
      mm = QuantumStack::decode_mmap("<MMap><map>"+KVP27+"</map><map>"+KVREX27+"</map></MMap>")
      mm.should == {27 => "rex"}
    end
    it "should create a map with all entries in the maps in the list" do
      mm = QuantumStack::decode_mmap("<MMap><map>"+KVP1+"</map><map>"+KVREX27+KVTH13+"</map></MMap>")
      mm.should == {1 => "p", 27 => "rex", 13 => "th"}
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