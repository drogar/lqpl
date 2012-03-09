require 'spec/spec_helper'

describe Compiler do
  context "when the compiler process is not running" do
    it "generates an error when created with defaults" do
      expect {
        Compiler.new
      }.to raise_error(
        CompilerProcessNotFound,
        /no process found on port 7863/
      )
    end
    it "generates an error when created with port > 1000" do
      expect {
        Compiler.new 3456
        }.to raise_error(
        CompilerProcessNotFound,
        /no process found on port 3456/
        )
    end
  end
  context "when the compiler process is running" do
    context "creation of the Compiler interface" do
      context "creation options for the compiler interface" do
        it "allows passing a port number > 1000 on creation" do
          @cmp = Compiler.new 3456
          @cmp.port.should == 3456
        end
        it "allows default creation with a port of 7863" do
          @cmp = Compiler.new
          @cmp.port.should == 7863
        end
      end
      context "once the compiler interface has been created" do
        before :each do
          @cmp = Compiler.new
        end 
        it "connects to the compiler process when created" do
          @cmp.should be_connected
        end
        it "generates an error if there is no compiler process"
      end
    end
    context "interfaces with the Compiler process" do
      it "reads data from a QPL file when given a filename"
      it "sends QPL code to the compiler process"
      it "receives QPO code from the compiler process"
      it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code" 
    end
  end
end