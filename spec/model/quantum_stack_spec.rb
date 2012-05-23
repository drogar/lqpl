require 'spec/spec_helper'

describe QuantumStack do
  describe "class method decode_mmap" do
    it "should create the map 1 -> p when input a kv xml with p as key and 1 as val" do
      mm = QuantumStack::decode_mmap("<MMap><map><kvpair><key><string>p</string></key><value><int>1</int></value></kvpair></map></MMap>")
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