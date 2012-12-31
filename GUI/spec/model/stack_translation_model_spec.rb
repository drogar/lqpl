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
      StackTranslationModel::kv_pairs_to_map(KVP1+KVREX27).should == {:p => 1, :rex => 27}
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
    }.to raise_error ModelCreateError, /err/
  end
  it "should create a text representation in its text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    s.text.should == "<html><ol><li>p=>1</li></ol></html>"
    s.stack_translation= L3STACK
    s.text.should == "<html><ol><li>p=>1</li><li>p=>2</li><li>rex=>27, p=>3</li></ol></html>"
  end
  it "should ignore direct assignment to the text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    s.text = "junk"
    s.text.should == "<html><ol><li>p=>1</li></ol></html>"
  end
  describe "it should provide reverse lookups" do
    before(:each) do
      @s = StackTranslationModel.new
    end
    it "should return the requested value if it is not found on reverse lookup" do
      @s.reverse_lookup(15).should == "15"
    end
    it "should return the name when there is only one entry" do
      @s.stack_translation = P1
      @s.reverse_lookup(1).should == "p"
    end
    it "should return the name of the first entry when there are repeated keys in multiple lists" do
      @s.stack_translation = P1ANDR1
      @s.reverse_lookup(1).should == "p"
      @s.stack_translation = P1ANDR1ANDS1
      @s.reverse_lookup(1).should == "p"
      @s.stack_translation = P1ANDEMPTYANDS1
      @s.reverse_lookup(1).should == "p"
    end
    it "should return the keys for all the values in the list of maps" do
      @s.stack_translation = L3STACK

      @s.reverse_lookup(1).should == "p"
      @s.reverse_lookup(2).should == "p"
      @s.reverse_lookup(3).should == "p"
      @s.reverse_lookup(27).should == "rex"
    end
  end
end

