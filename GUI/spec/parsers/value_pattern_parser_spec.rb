require 'spec/spec_helper'


describe ValuePatternParser do
  it "should successfully parse VALUE-number-VALUE '<Value>1.0</Value>'" do
    p = ValuePatternParser.new "<Value>1.0</Value>"
    expect(p.parsed?).to be_true
  end
  it "should successfully parse VALUE-NUMBER-number-NUMBER-VALUE '<Value><number>0.3846298001</number></Value>'" do
    p = ValuePatternParser.new "<Value>0.3846298001</Value>"
    expect(p.parsed?).to be_true
  end
  it "should successfully parse VALUE-NUMBER-enumber-NUMBER-VALUE '<Value>6.25e-2</Value>'" do
    p = ValuePatternParser.new "<Value>6.25e-2</Value>"
    expect(p.parsed?).to be_true
  end
  it "should raise an error if the value is > 1" do
    expect {
      ValuePatternParser.new "<Value>1.1</Value>"
    }.to raise_error ParserError, /1.1/
  end
  it "should raise an error if the value is < 0" do
    expect {
      ValuePatternParser.new "<Value>-0.5</Value>"
    }.to raise_error ParserError, /-0.5/
  end
  it "should raise an error for any other string" do
    expect {
      ValuePatternParser.new "<Value><Cint>4</Cint></Value>"
    }.to raise_error ParserError, /Value/
    expect {
      ValuePatternParser.new "whatever"
    }.to raise_error ParserError, /whatever/
  end
  context "parsed value" do
    it "should return the 1 for input <Value>1.0</Value>" do
      p = ValuePatternParser.new "<Value>1.0</Value>"
      expect(p.parsed_value).to eq(1.0)
    end
    it "should return the value 0.3846298001  for input '<Value><number>0.3846298001</number></Value>'" do
      p = ValuePatternParser.new '<Value><number>0.3846298001</number></Value>'
      expect(p.parsed_value).to eq(0.3846298001)
    end
    it "should return the value 6.25e-2  for input '<Value><number>6.25e-2</number></Value>'" do
      p = ValuePatternParser.new '<Value><number>6.25e-2</number></Value>'
      expect(p.parsed_value).to eq(6.25e-2)
    end
  end
end