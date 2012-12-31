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
end