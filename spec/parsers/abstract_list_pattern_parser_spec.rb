require 'spec/spec_helper'

describe AbstractListPatternParser do
  it "assigns [] to parsed_value on creation" do
    alpp = AbstractListPatternParser.new ""
    expect(alpp.parsed_value).to eq([])
  end
  describe "values_to_list" do
    it "returns [] with args a,/b/" do
      r = AbstractListPatternParser::values_to_list "a",/b/
      expect(r).to eq([])
    end
    it "returns {} with args a,/b/,{}" do
      r = AbstractListPatternParser::values_to_list("a",/b/,{})
      expect(r).to eq({})
    end
    it "yields [] and the match to a block" do
      AbstractListPatternParser::values_to_list "a",/a/ do |r,m|
        expect(r).to eq([])
        expect(m[0]).to eq("a")
      end
    end
    it "returns whatever elements the block adds to the first yield parameter" do
      expect(AbstractListPatternParser::values_to_list("a",/a/) { |r,m|
         r << 1
         r << 2
         r << 3
         }).to eq([1,2,3])
    end
    it "ignores assignments in the block to the first yield parameter" do
      expect(AbstractListPatternParser::values_to_list("a",/a/){|r,m| r = [1,2,3]}).to eq([])
    end
    it "successively yields further matches of repeated values" do
      expect(AbstractListPatternParser::values_to_list("abababab",/ab/) {|r,m| r << m[0]}).to eq(["ab","ab","ab","ab"])
    end
  end
end
      