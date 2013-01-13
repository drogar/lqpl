require 'spec/spec_helper'

require 'spec/specdata/dump_data'

describe DumpParser do
  it "should parse a single call dump" do
    dcp = DumpParser.new DUMPSINGLECALL
    dcp.parsed?.should be_true
  end
  it "should parse a single split dump" do
    dcp = DumpParser.new DUMPSPLIT
    dcp.parsed?.should be_true
  end
  it "should parse a list of call dump" do
    dcp = DumpParser.new DUMPTWOCALL
    dcp.parsed?.should be_true
  end
  it "should parse a mixed list of call and split dump" do
    dcp = DumpParser.new DUMPCALLSPLITCALL
    dcp.parsed?.should be_true
  end
end