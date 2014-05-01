require 'spec/spec_helper'
require 'spec/specdata/executing_code_data'

describe ExecutingCodeParser do
  describe "class method instructions_to_list" do
    it "should make an empty list when no pairs" do
      expect(ExecutingCodeParser::instructions_to_list("")).to eq([])
    end
    it "should make a singleton list when there is one <i> and </i> pair and prepend '  0  ' to the item" do
      expect(ExecutingCodeParser::instructions_to_list("<i>EnScope</i>")).to eq(["  0  EnScope"])
    end
    it "should make a list of all items between <i> and </i> pairs and prepend the index of the item " do
      expect(ExecutingCodeParser::instructions_to_list(KVPS6)).to eq(['  0  EnScope',
        '  1  QLoad "@q" 0',
        '  2  QApply 0 Hadamard "@q"',
        '  3  QPullup "@q"',
        '  4  EnScope',
        '  5  Measure "@q" 14 6 10'])
    end
    it "should properly justify indexes when the index > 0 " do
      expect(ExecutingCodeParser::instructions_to_list(KVPS6+KVPS6)[9,11]).to eq([
        '  9  QPullup "@q"',
        ' 10  EnScope',
        ' 11  Measure "@q" 14 6 10'])
    end
  end
  it "prepares an empty map when there are no keys" do
    ecp = ExecutingCodeParser.new '<Code><map></map></Code>'
    expect(ecp.parsed_value).to eq({})
  end
  it "prepares an one element map when there is only one key in the XML" do
    ecp = ExecutingCodeParser.new('<Code><map><kvpair><key><string>main</string></key>'+
      '<value><instructions><i>EnScope</i></instructions></value></kvpair></map></Code>')
    expect(ecp.parsed_value).to eq({:main => ["  0  EnScope"]})
  end
  it "prepares an two element map when there are two keys in the XML" do
    ecp = ExecutingCodeParser.new(CMAP_2)
    expect(ecp.parsed_value).to eq(RES_CMAP_2)
  end
  it "should throw an exception with bad input" do
    expect {ExecutingCodeParser.new "junk"}.to raise_error ParserError, /junk/
  end
end
