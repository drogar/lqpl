require 'spec/spec_helper'


describe AbstractPatternParser do
  it "should parse only an empty string" do
    p = AbstractPatternParser.new ""
    p.parsed?.should be_true
  end
  it "should raise an error for other string" do
    expect {
      AbstractPatternParser.new "<Zero/>"
    }.to raise_error ParserError, /Zero/
    expect {
      AbstractPatternParser.new "whatever"
    }.to raise_error ParserError, /whatever/
  end
  describe "class methods" do
    describe "surround_with_start_end" do
      it "when given the regexp 'ab' gives a regexp that only parses 'ab' " do
        AbstractPatternParser::surround_with_start_end(/ab/).should =~ 'ab'
      end
      it "when given the regexp 'ab' gives a regexp that will not  parses 'xaby' " do
        AbstractPatternParser::surround_with_start_end(/ab/).should_not =~ 'xaby'
      end
    end
  end
      
end