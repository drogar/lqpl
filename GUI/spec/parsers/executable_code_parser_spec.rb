require 'spec/spec_helper'
require 'spec/specdata/executable_code_data'

describe ExecutableCodeParser do
  describe "class method instructions_to_list" do
    it "should make an empty list when no pairs" do
      ExecutableCodeParser::instructions_to_list("").should == []
    end
    it "should make a singleton list when there is one <i> and </i> pair and prepend '  0  ' to the item" do
      ExecutableCodeParser::instructions_to_list("<i>EnScope</i>").should == ["  0  EnScope"]
    end
    it "should make a list of all items between <i> and </i> pairs and prepend the index of the item " do
      ExecutableCodeParser::instructions_to_list(KVPS6).should == ['  0  EnScope',
        '  1  QLoad "@q" 0',
        '  2  QApply 0 Hadamard "@q"',
        '  3  QPullup "@q"',
        '  4  EnScope',
        '  5  Measure "@q" 14 6 10']
    end
    it "should properly justify indexes when the index > 0 " do
      ExecutableCodeParser::instructions_to_list(KVPS6+KVPS6)[9,11].should == [
        '  9  QPullup "@q"',
        ' 10  EnScope',
        ' 11  Measure "@q" 14 6 10']
    end
  end
  it "prepares an empty map when there are no keys" do
    ecp = ExecutableCodeParser.new '<Code><map></map></Code>'
    ecp.parsed_value.should == {}
  end
  it "prepares an one element map when there is only one key in the XML" do
    ecp = ExecutableCodeParser.new('<Code><map><kvpair><key><string>main</string></key>'+
      '<value><instructions><i>EnScope</i></instructions></value></kvpair></map></Code>')
    ecp.parsed_value.should == {:main => ["  0  EnScope"]}
  end
  it "prepares an two element map when there are two keys in the XML" do
    ecp = ExecutableCodeParser.new(CMAP_2)
    ecp.parsed_value.should ==RES_CMAP_2
  end
  it "should throw an exception with bad input" do
    expect {ExecutableCodeParser.new "junk"}.to raise_error ParserError, /junk/
  end
end
