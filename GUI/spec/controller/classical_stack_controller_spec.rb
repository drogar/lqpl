require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'src/panels/classical_stack/classical_stack_controller'
require 'src/panels/classical_stack/classical_stack_model'

describe ClassicalStackController do
  before(:each) do
    SwingRunner::on_edt do
      @c = ClassicalStackController.instance
    end
  end
  it "should raise an error when created with junk" do
    expect { @c.set_classical_stack_data("junk")}. to raise_error   ParserError, /junk/
  end
  
  specify {@c.update_on_lqpl_model_trim.should be_false}
  it "should create a classical stack when given the correct input" do
    SwingRunner::on_edt do
      @c.set_classical_stack_data("<Classical>"+cint(-27)+CIBT+cint(40)+CIBF+"</Classical>")
      @c.get_classical_stack_data.should == "<html>-27<br />true<br />40<br />false</html>"
    end
  end
end