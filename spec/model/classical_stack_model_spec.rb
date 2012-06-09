require 'spec/spec_helper'

require 'spec/specdata/classical_stack_data'
require 'src/panels/classical_stack/classical_stack_model'

describe ClassicalStackModel do
  it "should ignore the stack text being set" do
    c = ClassicalStackModel.new
    c.classical_stack_text = "junk"
    c.classical_stack_text.should == ""
  end
  it "should throw an error if given invalid input" do
    expect {
      c=ClassicalStackModel.new
      c.classical_stack = "junk"
    }.to raise_error  QuantumStackModelInvalidCreate, /junk/
  end
  it "should set the text to each item with line returns" do
    c=ClassicalStackModel.new
    c.classical_stack ="<Cstack>"+cint(-27)+CIBT+cint(40)+CIBF+"</Cstack>"
    c.classical_stack_text.should == "<html>-27<br />true<br />40<br />false</html>"
  end
  describe "class method classical_values_to_list" do
    it "should make an empty list with no data" do
      ClassicalStackModel.classical_values_to_list("").should == []
    end
    it "should make an empty list with nil data" do
      ClassicalStackModel.classical_values_to_list(nil).should == []
    end
    it "should make a singleton list when there is either a single int or bool" do
      ClassicalStackModel.classical_values_to_list(cint(-27)).should == [-27]
      ClassicalStackModel.classical_values_to_list(CIBT).should == [true]
      ClassicalStackModel.classical_values_to_list(CIBF).should == [false]
    end
    it "should make a list of all items: ints and bools" do
      ClassicalStackModel.classical_values_to_list(cint(-27)+CIBT+cint(40)+CIBF).should == [-27,true,40,false]
    end
  end


end