require 'spec/spec_helper'

describe CompilerServerConnection do

  context "helper functions" do
    context "makeVersionNumber" do
      it "takes the input CS_VERSION [0,8,4] [] and returns 0.8.4" do
        res = CompilerServerConnection::makeVersionNumber("CS_VERSION [0,8,4] []")
        res.should == "0.8.4"
      end
    end
  end

  context "creation" do
    context "enforce singleton" do
      before(:each) do
        @cmp = nil
      end
      after(:each) do
        @cmp.close_down if @cmp
      end
      it "gives an error when trying to create with 'new'" do
        expect {@cmp=CompilerServerConnection.new}.to raise_error NoMethodError
      end
      it "allows passing a port number > 1000 on creation" do
        @cmp = CompilerServerConnection.get_instance 3456
        @cmp.port.should == 3456
      end
      it "allows default creation with a port of 7683" do
        @cmp = CompilerServerConnection.get_instance
        @cmp.port.should == 7683
      end
    end
    context "connection" do
      before :each do
        @cmp = CompilerServerConnection.get_instance
      end
      after(:each) do
        @cmp.close_down if @cmp
      end
      it "connects to the lqpl-compiler process when created" do
        @cmp.connect
        @cmp.should be_connected
      end
    end
  end
  context "interfaces with the lqpl-compiler-server" do
    before :each do
        @cmp = CompilerServerConnection.get_instance
        @cmp.connect
      end
    it "sends QPL code to the lqpl-compiler-server and gets qpo code back" do
      fname = "#{Dir.pwd}/testdata/qplprograms/min.qpl"
      qpocode = @cmp.compile fname
      qpocode.should =~ /app_fcdlbl.*/
    end
    it "signals failure when the code has a syntax error" do
      @cmp.compile "#{Dir.pwd}/testdata/qplprograms/invalidsyntax.qpl"
      @cmp.failed.should be_true
      @cmp.failure_message.should =~ /unexpected/
    end
    it "signals failure when the code has a semantic error" do
      @cmp.compile "#{Dir.pwd}/testdata/qplprograms/invalidsemantics.qpl"
      @cmp.failed.should be_true
      @cmp.failure_message.should =~ /Semantic Error/
    end
    it "signals a warning when the code has a balance creation error" do
      fname = "#{Dir.pwd}/testdata/qplprograms/invalidbalance.qpl"
      @cmp.compile fname
      @cmp.failed.should be_false
      @cmp.failure_message.should =~ /Semantic Warning/
    end
    it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code" do
      fname = "#{Dir.pwd}/testdata/qplprograms/min.qpl"
      begin
        File.delete("#{Dir.pwd}/testdata/qplprograms/min.qpo")
      rescue
      end
      @cmp.compile fname
      @cmp.failed.should be_false
      @cmp.failure_message.should == ""
      @cmp.write_qpo_file
      File.exist?("#{Dir.pwd}/testdata/qplprograms/min.qpo").should be_true
      File.open("#{Dir.pwd}/testdata/qplprograms/min.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{Dir.pwd}/testdata/qplprograms/min.qpo") do |new_compile|
          new_data = new_compile.read
          new_data.should == ref_data
        end
      end
    end
    it "writes a .qpo file with the same name as the original .qpl file when imports are involved" do
      fname = "#{Dir.pwd}/testdata/qplprograms/importer.qpl"
      begin
        File.delete("#{Dir.pwd}/testdata/qplprograms/importer.qpo")
      rescue
      end
      @cmp.compile fname
      @cmp.write_qpo_file
      File.exist?("#{Dir.pwd}/testdata/qplprograms/importer.qpo").should be_true
      File.open("#{Dir.pwd}/testdata/qplprograms/importer.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{Dir.pwd}/testdata/qplprograms/importer.qpo") do |new_compile|
          new_data = new_compile.read
          new_data.should == ref_data
        end
      end
    end
  end
end