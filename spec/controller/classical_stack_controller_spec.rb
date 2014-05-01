require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'GUI/src/panels/classical_stack/classical_stack_controller'
require 'GUI/src/panels/classical_stack/classical_stack_model'

describe ClassicalStackController do
  before(:each) do
    SwingRunner::on_edt do
      @c = ClassicalStackController.instance
    end
  end
  it "should raise an error when created with junk" do
    expect { @c.update_classical_stack_data("junk")}. to raise_error   ParserError, /junk/
  end

  specify {expect(@c.update_on_lqpl_model_trim).to be false}
  it "should create a classical stack when given the correct input" do
    SwingRunner::on_edt do
      @c.update_classical_stack_data("<Classical>"+cint(-27)+CIBT+cint(40)+CIBF+"</Classical>")
      expect(@c.classical_stack_data).to eq("<html>-27<br />true<br />40<br />false</html>")
    end
  end
end