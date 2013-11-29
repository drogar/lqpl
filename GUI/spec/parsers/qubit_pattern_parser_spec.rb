require 'spec/spec_helper'


describe QubitPatternParser do
  it  "should raise an error if constructed with something other than <Qubits>list of z,o pairs</Qubits>" do
    expect {
      QubitPatternParser.new "<Qubits>err</Qubits>"
    }.to raise_error(ParserError, /<Qubits>err<\/Qubits>/)
  end
  it "should have the value being the list of qubit indicators in the string" do
    qpp = QubitPatternParser.new "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    expect(qpp.parsed_value).to eq([[0,0],[0,1],[1,0],[1,1]])
  end
  context "list matching" do
    it "should result in a one element pair list when that is the only input" do
      qpp = QubitPatternParser.new "<Qubits><pair><qz/><qz/></pair></Qubits>"
      expect(qpp.parse_list).to eq([[0,0]])
    end
    it "should result in a mixed list when that is the same input" do
      qpp = QubitPatternParser.new "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair></Qubits>"
      expect(qpp.parse_list).to eq([[0,0],[0,1]])
    end
    it "should raise an error when the list has duplicated pairs" do
      expect {
        QubitPatternParser.new "<Qubits><pair><qz/><qz/></pair><pair><qz/><qz/></pair></Qubits>"
       # qpp.parse_list("<pair><qz/><qz/></pair><pair><qz/><qz/></pair>")
      }.to raise_error(InvalidInput, "[0, 0] duplicated in qubit")
    end
  end
  context "translate qubit" do
    it "should return 1 for <qo/>" do
      expect(QubitPatternParser::translate_qubit("<qo/>")).to eq(1)
    end
    it "should return 0 for <qz/>" do
      expect(QubitPatternParser::translate_qubit("<qz/>")).to eq(0)
    end
    it "should raise an exception for any other input" do
      expect {
        QubitPatternParser::translate_qubit("err")
      }.to raise_error InvalidInput, /err/
    end
  end
end