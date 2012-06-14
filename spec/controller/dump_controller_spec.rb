require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'src/panels/dump/dump_controller'
require 'src/panels/dump/dump_model'

describe DumpController do
  before(:each) do
    @c = DumpController.instance
  end
  it "should accessibly store the dump text in the model" do
    @c.set_dump_data("<Dump>"+DCALL+"</Dump>")
    @c.get_dump_data.should == "<html><ol><li>Return to Ret(5). CS=[]</li></ol></html>"
  end

  it "should set the server_connection when given an sc" do

    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    @c.server_connection=sc
  end
  it "should ask the sc for the dump when given a depth and recursion" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    sc.should_receive(:get_dump).and_return("<Dump></Dump>")

    @c.server_connection=sc
    @c.set_dump("5","4")
  end
end