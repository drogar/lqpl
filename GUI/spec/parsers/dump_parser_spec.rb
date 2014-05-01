require 'spec/spec_helper'

require 'spec/specdata/dump_data'

describe DumpParser do
  it "should parse a single call dump" do
    dcp = DumpParser.new DUMPSINGLECALL
    expect(dcp.parsed?).to be true
  end
  it "should parse a single split dump" do
    dcp = DumpParser.new DUMPSPLIT
    expect(dcp.parsed?).to be true
  end
  it "should parse a list of call dump" do
    dcp = DumpParser.new DUMPTWOCALL
    expect(dcp.parsed?).to be true
  end
  it "should parse a mixed list of call and split dump" do
    dcp = DumpParser.new DUMPCALLSPLITCALL
    expect(dcp.parsed?).to be true
  end
end