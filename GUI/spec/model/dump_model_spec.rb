require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'src/panels/dump/dump_model'

describe DumpCallModel do
  it "should accessibly store the dumpcall data in the model" do
    dc = DumpCallModel.new(DCALL)
    dc.text.should == "Return to Ret(5). CS=[]"
  end
end

describe DumpSplitModel do
  it "should just accept and display the text of the split" do
    ds = DumpSplitModel.new( "<DumpSplit>whatever</DumpSplit>")
    ds.text.should == "whatever"
  end
end

describe DumpModel do
  it "should accessibly store the dump call data in the model" do
    d = DumpModel.new
    dc = DumpCallModel.new(DCALL)
    d.dump = "<Dump>"+DCALL+"</Dump>"
    d.dump[0].text.should == dc.text
  end
  it "should create a list of dumpcall and dumpsplit items" do
    dc=DumpModel.new
    dc.dump="<Dump>"+DCALL+DSPLIT+DCALL2+"</Dump>"
    dc.should have(3).dump
    dc.dump[0].class.should == DumpCallModel
    dc.dump[1].class.should == DumpSplitModel
  end
end