require 'spec/spec_helper'


describe DataPatternParser do
  it  "should raise an error if constructed with something other than <AlgebraicData>list of pairs</AlgebraicData>" do
    expect {
      DataPatternParser.new "<AlgebraicData>err</AlgebraicData>"
    }.to raise_error(ParserError, /<AlgebraicData>err<\/AlgebraicData>/)
  end
  context "constructor matching" do
    it "should raise an error when the constructor map is invalid" do
      expect {
        DataPatternParser.new("<AlgebraicData>invalid<\/AlgebraicData>")
      }.to raise_error(ParserError, /invalid/)
    end
    it "should raise an error when there is no constructor" do
      expect {
        DataPatternParser.new("<AlgebraicData><\/AlgebraicData>")
      }.to raise_error(ParserError, /No match/)
    end
    it "should result in a one element array of pairs when that is the only input" do
      dpp = DataPatternParser.new("<AlgebraicData><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses><\/AlgebraicData>")
      expect(dpp.parsed_value).to eq([["C",[3,4]]])
    end
    it "should result in a mixed map when that is the input" do
      dpp = DataPatternParser.new("<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses><\/AlgebraicData>")
      expect(dpp.parsed_value).to eq([["Nil",[]], ["C" , [3,4]]])
    end
    it "should raise an error when the map has duplicated constructors" do
      expect {
        DataPatternParser.new("<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>Nil</string><StackAddresses></StackAddresses><\/AlgebraicData>")
      }.to raise_error(InvalidInput, "Constructor 'Nil' duplicated in algebraic data")
    end
  end
  context "address list matching" do
    it "should result in an empty list when there is no input" do
      expect(DataPatternParser::parse_address_list("")).to eq([])
    end
    it "should result in a one element list when that is the only input" do
      expect(DataPatternParser::parse_address_list("<int>3</int>")).to eq([3])
    end
    it "should result in a many element list when that is the input" do
      expect(DataPatternParser::parse_address_list("<int>3</int><int>4</int><int>5</int><int>6</int><int>7</int>")).to eq([3,4,5,6,7])
    end
    it "should raise an error when the list has duplicated elements" do
      expect {
        DataPatternParser::parse_address_list("<int>3</int><int>3</int>")
      }.to raise_error(InvalidInput, "StackAddress '3' duplicated for single constructor")
    end
  end
end