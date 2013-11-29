require 'spec/spec_helper'


describe ClassicalPatternParser do
  it  "should raise an error if constructed with something other than <Classical>list of ints or bools</Classical>" do
    expect {
      ClassicalPatternParser.new "<Classical>err</Classical>"
    }.to raise_error(ParserError, /<Classical>err<\/Classical>/)
  end
  
  context "list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        ClassicalPatternParser.new("<Classical>invalid<\/Classical>")
      }.to raise_error(ParserError, /invalid/)
    end
    it "should result in a one element int list [1] when that is the only input" do
      cpp= ClassicalPatternParser.new("<Classical><cint>1</cint><\/Classical>")
      expect(cpp.parsed_value).to eq([1])
    end
    it "should result in a one element int list [-3491] when that is the only input" do
      cpp= ClassicalPatternParser.new("<Classical><cint>-3491</cint><\/Classical>")
      expect(cpp.parsed_value).to eq([-3491])
    end
    it "should result in a one element bool list [true] when that is the only input" do
      cpp= ClassicalPatternParser.new("<Classical><cbool>True</cbool><\/Classical>")
      expect(cpp.parsed_value).to eq([true])
    end
    it "should result in a one element int list [false] when that is the only input" do
      cpp= ClassicalPatternParser.new("<Classical><cbool>False</cbool><\/Classical>")
      expect(cpp.parsed_value).to eq([false])
    end
    it "should result in a mixed list [1,true,14] when that is the input" do
      cpp= ClassicalPatternParser.new("<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint><\/Classical>")
      expect(cpp.parsed_value).to eq([1,true,14])
    end
  end
end