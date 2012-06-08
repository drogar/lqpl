require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'

require 'src/panels/stack_translation/stack_translation_model'

describe StackTranslationModel do
    describe "class method kv_pairs_to_map" do
    it "should return the empty map if given nil" do
      StackTranslationModel::kv_pairs_to_map(nil).should be_empty
    end
    it "should return the empty map if there are no pairs" do
      StackTranslationModel::kv_pairs_to_map("").should be_empty
    end
    it "should return a  a key ->value  map of a single entry" do
      StackTranslationModel::kv_pairs_to_map(KVP1).should == {:p => 1}
    end
    it "should return  a key ->value map of all entries " do
      StackTranslationModel::kv_pairs_to_map(KVP27+KVREX27).should == {:p => 27, :rex => 27}
    end

  end
  describe "class method decode_mmap" do
    it "should create a one element list when one map input" do
      mm = StackTranslationModel::decode_mmap(P1)
      mm.should == [{:p => 1}]
    end
    it "should create the two element list of empty map and one elt map when input empty and then p1" do
      mm = StackTranslationModel::decode_mmap(P1ANDEMPTY)
      mm.should == [{},{:p => 1}]
    end
    it "should create a three element list with all map items when input a 3 level translation" do
      mm = StackTranslationModel::decode_mmap(L3STACK)
      mm.should == [{:p => 1},{:p => 2},{:rex => 27,:p => 3}]
    end
  end

  it "should give an invalid create error when sent incorrect data" do
    expect {
      s = StackTranslationModel.new
      s.stack_translation= "err"
    }.to raise_error QuantumStackModelInvalidCreate, /err/
  end
  it "should create a text representation in its text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    s.text.should == "[{:p=>1}]"
    s.stack_translation= L3STACK
    s.text.should == "[{:p=>1}, {:p=>2}, {:rex=>27, :p=>3}]"
  end
  it "should ignore direct assignment to the text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    s.text = "junk"
    s.text.should == "[{:p=>1}]"
  end
end

