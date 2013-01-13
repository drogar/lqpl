require 'spec/spec_helper'

describe AbstractListPatternParser do
  it "assigns [] to parsed_value on creation" do
    alpp = AbstractListPatternParser.new ""
    alpp.parsed_value.should == []
  end
  describe "values_to_list" do
    it "returns [] with args a,/b/" do
      r = AbstractListPatternParser::values_to_list "a",/b/
      r.should == []
    end
    it "returns {} with args a,/b/,{}" do
      r = AbstractListPatternParser::values_to_list("a",/b/,{})
      r.should == {}
    end
    it "yields [] and the match to a block" do
      AbstractListPatternParser::values_to_list "a",/a/ do |r,m|
        r.should == []
        m[0].should == "a"
      end
    end
    it "returns whatever elements the block adds to the first yield parameter" do
      (AbstractListPatternParser::values_to_list "a",/a/ do |r,m|
         r << 1
         r << 2
         r << 3
       end).should == [1,2,3]
    end
    it "ignores assignments in the block to the first yield parameter" do
      (AbstractListPatternParser::values_to_list "a",/a/ do |r,m|
         r = [1,2,3]
       end).should == []
    end
    it "successively yields further matches of repeated values" do
      (AbstractListPatternParser::values_to_list "abababab",/ab/ do |r,m|
         r << m[0]
       end).should == ["ab","ab","ab","ab"]
    end
  end
end
      