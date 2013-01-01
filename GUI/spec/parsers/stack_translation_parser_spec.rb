require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'


describe StackTranslationParser do
    describe "class method kv_pairs_to_map" do
    it "should return the empty map if there are no pairs" do
      stp = StackTranslationParser.new(EMPTYSTACK)
      stp.parsed_value.should == [{}]
    end
    it "should return a  a key ->value  map of a single entry" do
      stp = StackTranslationParser.new(P1)
      stp.parsed_value.should == [{:p => 1}]
    end
    it "should return  a key ->value map of all entries " do
      stp = StackTranslationParser.new(P1WITHREX27)
      stp.parsed_value.should == [{:p => 1, :rex => 27}]
    end
    it "should create the two element list of empty map and one elt map when input empty and then p1" do
      stp = StackTranslationParser.new(P1ANDEMPTY)
      stp.parsed_value.should == [{},{:p => 1}]
    end
    it "should create a three element list with all map items when input a 3 level translation" do
      stp = StackTranslationParser.new(L3STACK)
      stp.parsed_value.should == [{:p => 1},{:p => 2},{:rex => 27,:p => 3}]
    end
  end
end

