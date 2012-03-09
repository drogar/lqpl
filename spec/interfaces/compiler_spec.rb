require 'spec/spec_helper'

describe Compiler do
  context "creation of the Compiler" do
    it "connects to the compiler process when created"
    it "generates an error if there is no compiler process"
  end
  context "interfaces with the Compiler process" do
    it "reads data from a QPL file when given a filename"
    it "sends QPL code to the compiler process"
    it "receives QPO code from the compiler process"
    it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code" 
  end
end