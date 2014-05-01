require 'spec/spec_helper'

require 'spec/specdata/dump_data'

describe DumpCallParser do
  it "should successfully parse a call with no entries in the classical stack" do
    p = DumpCallParser.new DCALL
    expect(p.parsed?).to be true
  end
  it "should successfully parse a call with 1 entry in the classical stack" do
    p = DumpCallParser.new DCALL1
    expect(p.parsed?).to be true
  end
  
  it "should successfully parse a call with 2 entries in the classical stack" do
    p = DumpCallParser.new DCALL2
    expect(p.parsed?).to be true
  end
  it "should give a parse error if the int is missing" do
    expect {
      DumpCallParser.new "<DumpCall><string>Ret</string><Classical></Classical></DumpCall>"
    }.to raise_error ParserError, /DumpCall/
  end
  
  it "should give a parse error if the return string is missing" do
    expect {
      DumpCallParser.new "<DumpCall><int>5</int><Classical></Classical></DumpCall>"
    }.to raise_error ParserError, /DumpCall/
  end
  
  it "should give a parse error if the classical stack is missing" do
    expect {
      DumpCallParser.new "<DumpCall><int>5</int><string>Ret</string></DumpCall>"
    }.to raise_error ParserError, /DumpCall/
  end
  it "should give a parse error if the classical stack is incorrect" do
    expect {
      DumpCallParser.new "<DumpCall><int>5</int><string>Ret</string><Classical>junk</Classical></DumpCall>"
    }.to raise_error ParserError, /DumpCall/
  end
end
  