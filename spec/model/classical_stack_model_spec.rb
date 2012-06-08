require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'src/panels/classical_stack/classical_stack_model'

describe ClassicalStackModel do
  it "should accessibly store the stack text in the model" do
    c = ClassicalStackModel.new
    c.classical_stack_text = "junk"
    c.classical_stack_text.should == "junk"
  end
end