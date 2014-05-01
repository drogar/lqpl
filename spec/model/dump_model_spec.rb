require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'GUI/src/panels/dump/dump_model'

describe DumpCallModel do
  it "should accessibly store the dumpcall data in the model" do
    dc = DumpCallModel.new(DCALL)
    expect(dc.text).to eq("Return to Ret(5). CS=[]")
  end
end

describe DumpSplitModel do
  it "should just accept and display the text of the split" do
    ds = DumpSplitModel.new( "<DumpSplit>whatever</DumpSplit>")
    expect(ds.text).to eq("whatever")
  end
end

describe DumpModel do
  it "should accessibly store the dump call data in the model" do
    d = DumpModel.new
    dc = DumpCallModel.new(DCALL)
    d.dump = "<Dump>"+DCALL+"</Dump>"
    expect(d.dump[0].text).to eq(dc.text)
  end
  it "should create a list of dumpcall and dumpsplit items" do
    dc=DumpModel.new
    dc.dump="<Dump>"+DCALL+DSPLIT+DCALL2+"</Dump>"
    expect(dc.dump.length).to eql(3)
    expect(dc.dump[0].class).to eq(DumpCallModel)
    expect(dc.dump[1].class).to eq(DumpSplitModel)
  end
end