require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'
require 'spec/specdata/quantum_stack_data'
require 'src/panels/quantum_stack/quantum_stack_model'





describe QuantumStackModel do

  describe "class method kv_pairs_to_map" do
    it "should return the empty map if given nil" do
      QuantumStackModel::kv_pairs_to_map(nil).should be_empty
    end
    it "should return the empty map if there are no pairs" do
      QuantumStackModel::kv_pairs_to_map("").should be_empty
    end
    it "should return a value -> key map of a single entry" do
      QuantumStackModel::kv_pairs_to_map(KVP1).should == {1 => "p"}
    end
    it "should return a value -> key map with the last entry winning" do
      QuantumStackModel::kv_pairs_to_map(KVP27+KVREX27).should == {27 => "rex"}
    end

    it "should return a value -> key map with the all entries if no dup values" do
      QuantumStackModel::kv_pairs_to_map(KVP1+KVREX27+KVTH13).should == {1 => "p", 27 => "rex", 13 => "th"}
    end

  end
  describe "class method decode_mmap" do
    it "should create the map 1 -> p when input a kv xml with p as key and 1 as val" do
      mm = QuantumStackModel::decode_mmap(P1)
      mm.should == {1 => "p"}
    end
     it "should create the map 1 -> p when input a kv xml with p as key and 1 as val when there is an empty map in front" do
        mm = QuantumStackModel::decode_mmap(P1ANDEMPTY)
        mm.should == {1 => "p"}
      end
    it "should create the map 1 -> p, 2 -> rex when input a kv xml with p as key and 1 as val and rex ->2 in same list" do
      mm = QuantumStackModel::decode_mmap("<MMap><map>"+KVP1REX2+"</map></MMap>")
      mm.should == {1 => "p", 2 => "rex"}
    end
    it "should create a map with last element of list if duplicated" do
      mm = QuantumStackModel::decode_mmap("<MMap><map>"+KVP27+"</map><map>"+KVREX27+"</map></MMap>")
      mm.should == {27 => "rex"}
    end
    it "should create a map with all entries in the maps in the list" do
      mm = QuantumStackModel::decode_mmap("<MMap><map>"+KVP1+"</map><map>"+KVREX27+KVTH13+"</map></MMap>")
      mm.should == {1 => "p", 27 => "rex", 13 => "th"}
    end
    it "Should handle a simple 2 element map" do
      QuantumStackModel::decode_mmap(Q1R2).should == {1 =>"@q", 2 => "@r"}
    end
  end
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
    it "should create an empty array for no input" do
      rv  = QuantumStackModel::make_multiple_stacks("")
      rv.length.should == 0
    end
    it "should create a one element array for one" do
      rv = QuantumStackModel::make_multiple_stacks(""+QSVAL5+"")
      rv.length.should == 1
    end
    it "should create an n element array for n copies" do
      [2,3,4,5,6].each do |i|
        rv = QuantumStackModel::make_multiple_stacks((""+QSVAL5+"")*i)
        rv.length.should == i
      end
    end
  end
  it "should give an invalid create error when created with incorrect data" do
    expect {
      qs = QuantumStackModel.new "err"
    }.to raise_error QuantumStackModelInvalidCreate, /err/
  end
  it "should give an error when stackzero has substacks" do
    expect {
      qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
    }.to raise_error QuantumStackModelInvalidCreate, /not have/
  end
  it "should give an error when stackvalue has substacks" do
    expect {
      qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Value>0.5</Value></Qstack>"
    }.to raise_error QuantumStackModelInvalidCreate, /not have/
  end
  it "should give an error when stackqubit does not have substacks" do
    expect {
      qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    }.to raise_error QuantumStackModelInvalidCreate, /should have/
  end
  it "should give an error when stackclassical does not have substacks"  do
    expect {
      qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><ClassicalStack><cint>14</cint></ClassicalStack></Qstack>"
    }.to raise_error QuantumStackModelInvalidCreate, /should have/
  end
  it "should give an error when stackdata does not have substacks" do
    expect {
      qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"
    }.to raise_error QuantumStackModelInvalidCreate, /should have/
  end
  it "should successfully create the start qstack" do
    qs = QuantumStackModel.new "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack>"
    qs.should_not be_bottom
  end
  it "should allow 'bottom' as the construction"  do
    qs = QuantumStackModel.new "<bottom/>"
    qs.should be_bottom
  end
  it "should allow 'bottom' in place of substacks"  do
    qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
  end
  it "should have the same number of substacks as the length of the descriptor" do
    qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    qs.substacks.length.should == 1
    qs = QuantumStackModel.new "<Qstack><int>1</int><bool>True</bool><substacks><bottom/><bottom/><bottom/></substacks><ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack></Qstack>"
    qs.substacks.length.should == 3
  end
  it "should assign a name for a quantum descriptor" do
    qs = QuantumStackModel.new("<Qstack><int>2</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>",
    "<MMap><map><kvpair><key><string>p</string></key><value><int>2</int></value></kvpair></map></MMap>")
    qs.descriptor.name.should == "p"
  end
  it "should assign names to multi-level qstacks" do
    qs = QuantumStackModel.new(QSQ1R2,Q1R2)
    qs.descriptor.name.should == "@r"
    qs.substacks[0].descriptor.name.should == "@q"
  end
end