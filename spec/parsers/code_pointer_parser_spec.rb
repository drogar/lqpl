require 'spec/spec_helper'
describe CodePointerParser do
  it "should return the created code map when given correct input when there is code" do
    cpp = CodePointerParser.new("<pair><string>main</string><int>0</int></pair>")
    expect(cpp.parsed_value).to eq([:main,0])
  end

  it "should throw an exception with bad input" do
    expect {CodePointerParser.new("junk")}.to raise_error ParserError, /junk/
  end
  it "should throw an exception with nil input" do
    expect {CodePointerParser.new(nil)}.to raise_error ParserError, /No match/
  end
  it "should throw an exception with blank input" do
    expect {CodePointerParser.new("")}.to raise_error ParserError, /No match/
  end
end
