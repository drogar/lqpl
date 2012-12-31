require 'spec/spec_helper'


describe ClassicalPatternParser do
  it  "should raise an error if constructed with something other than <ClassicalStack>list of ints or bools</ClassicalStack>" do
    expect {
      ClassicalPatternParser.new "<ClassicalStack>err</ClassicalStack>"
    }.to raise_error(ParserError, /<ClassicalStack>err<\/ClassicalStack>/)
  end
  
  context "list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        ClassicalPatternParser.new("<ClassicalStack>invalid<\/ClassicalStack>")
      }.to raise_error(ParserError, /invalid/)
    end
    it "should result in a one element int list [1] when that is the only input" do
      cpp= ClassicalPatternParser.new("<ClassicalStack><cint>1</cint><\/ClassicalStack>")
      cpp.parsed_value.should == [1]
    end
    it "should result in a one element int list [-3491] when that is the only input" do
      cpp= ClassicalPatternParser.new("<ClassicalStack><cint>-3491</cint><\/ClassicalStack>")
      cpp.parsed_value.should == [-3491]
    end
    it "should result in a one element bool list [true] when that is the only input" do
      cpp= ClassicalPatternParser.new("<ClassicalStack><cbool>True</cbool><\/ClassicalStack>")
      cpp.parsed_value.should == [true]
    end
    it "should result in a one element int list [false] when that is the only input" do
      cpp= ClassicalPatternParser.new("<ClassicalStack><cbool>False</cbool><\/ClassicalStack>")
      cpp.parsed_value.should == [false]
    end
    it "should result in a mixed list [1,true,14] when that is the input" do
      cpp= ClassicalPatternParser.new("<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint><\/ClassicalStack>")
      cpp.parsed_value.should == [1,true,14]
    end
  end
end