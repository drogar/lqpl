require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'src/panels/classical_stack/classical_stack_controller'
require 'src/panels/classical_stack/classical_stack_model'

describe ClassicalStackController do
  before(:each) do
    @c = ClassicalStackController.instance
  end
  it "should accessibly store the stack text in the model" do
    @c.set_classical_stack_data("junk")
    @c.get_classical_stack_data.should == "junk"
  end
  it "should set the server_connection when given an sc" do

    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    @c.server_connection=sc
  end
  it "should ask the sc for the classical stack when given a depth and recursion" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    sc.should_receive(:get_classical_stack).and_return("<Cstack></Cstack>")

    @c.server_connection=sc
    @c.set_classical_stack("5","4")
  end
end