require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'src/panels/classical_stack/classical_stack_controller'
require 'src/panels/classical_stack/classical_stack_model'

describe ClassicalStackController do
  before(:each) do
    @c = ClassicalStackController.instance
  end
  it "should raise an error when created with junk" do
    expect { @c.set_classical_stack_data("junk")}. to raise_error   QuantumStackModelInvalidCreate, /junk/
  end

  it "should create a classical stack when given the correct input" do
    @c.set_classical_stack_data("<Cstack>"+cint(-27)+CIBT+cint(40)+CIBF+"</Cstack>")
    @c.get_classical_stack_data.should == "<html>-27<br />true<br />40<br />false</html>"
  end

  it "should set the server_connection when given an sc" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    @c.server_connection=sc
  end
  it "should ask the sc for the classical stack when given a depth and recursion" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    sc.should_receive(:get_classical_stack).and_return("<Cstack>"+CIBT+"</Cstack>")

    @c.server_connection=sc
    @c.set_classical_stack("5","4")
    @c.get_classical_stack_data.should == "<html>true</html>"
  end
end