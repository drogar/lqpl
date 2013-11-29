require 'spec/spec_helper'


describe ZeroPatternParser do
  it "should successfully parse only '<Zero/>'" do
    p = ZeroPatternParser.new "<Zero/>"
    expect(p.parsed?).to be_true
  end
  it "should raise an error for any other string" do
    expect {
      ZeroPatternParser.new "<Value><Cint>4</Cint></Value>"
    }.to raise_error ParserError, /Value/
    expect {
      ZeroPatternParser.new "whatever"
    }.to raise_error ParserError, /whatever/
  end
end