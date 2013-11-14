require 'spec/spec_helper'


describe DumpSplitParser do
  it "should successfully parse  '<DumpSplit>1.0</DumpSplit>'" do
    p = DumpSplitParser.new "<DumpSplit>1.0</DumpSplit>"
    p.parsed?.should be_true
  end
  
  it "should successfully parse  '<DumpSplit>whatever</DumpSplit>'" do
    p = DumpSplitParser.new "<DumpSplit>whatever</DumpSplit>"
    p.parsed?.should be_true
  end
  it "should successfully parse  '<DumpSplit>what<DumpSplit>whatever</DumpSplit>ever</DumpSplit>'" do
    p = DumpSplitParser.new "<DumpSplit>what<DumpSplit>whatever</DumpSplit>ever</DumpSplit>"
    p.parsed?.should be_true
  end
  it "should parse  '<DumpSplit>what<DumpSplit>whatever</DumpSplit>ever</DumpSplit>' as 'what<DumpSplit>whatever</DumpSplit>ever'" do
    p = DumpSplitParser.new "<DumpSplit>what<DumpSplit>whatever</DumpSplit>ever</DumpSplit>"
    p.parsed_value.should == "what<DumpSplit>whatever</DumpSplit>ever"
  end
end