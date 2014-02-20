require 'spec/spec_helper'


describe CompilerServerConnection do

  context "helper functions" do
    context "make_version_number" do
      it "takes the input CS_VERSION [0,8,4] [] and returns 0.8.4" do
        res = CompilerServerConnection::make_version_number("CS_VERSION [0,8,4] []")
        expect(res).to eq("0.8.4")
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
      # it "allows passing a port number > 1000 on creation" do
      #   @cmp = CompilerServerConnection.get_instance 3456
      #   @cmp.port.should == 3456
      # end
      it "allows default creation with a port of 7683" do
        @cmp = CompilerServerConnection.get_instance
        expect(@cmp.port).to eq(7683)
      end
    end
    context "connection" do
      before :each do
        @cmp = CompilerServerConnection.get_instance
      end
      after(:all) do
        @cmp.close_down if @cmp
      end
      it "connects to the lqpl-compiler process when created" do
        @cmp.connect
        expect(@cmp).to be_connected
      end
    end
  end
  context "interfaces with the lqpl-compiler-server" do
    before :each do
      @cmp = CompilerServerConnection.get_instance
      @cmp.connect
    end
    after(:all) do
      @cmp.close_down if @cmp
    end
    it "sends QPL code to the lqpl-compiler-server and gets qpo code back" do
      fname = "#{TEST_QP_PATH}/min.qpl"
      qpocode = @cmp.compile fname
      expect(qpocode).to match(/app_fcdlbl.*/)
    end
    it "signals failure when the code has a syntax error" do
      @cmp.compile "#{TEST_QP_PATH}/invalidsyntax.qpl"
      expect(@cmp.failed).to be_true
      expect(@cmp.failure_message).to match(/unexpected/)
    end
    it "signals failure when the code has a semantic error" do
      @cmp.compile "#{TEST_QP_PATH}/invalidsemantics.qpl"
      expect(@cmp.failed).to be_true
      expect(@cmp.failure_message).to match(/Semantic Error/)
    end
    it "signals a warning when the code has a balance creation error" do
      fname = "#{TEST_QP_PATH}/invalidbalance.qpl"
      @cmp.compile fname
      expect(@cmp.failed).to be_false
      expect(@cmp.failure_message).to match(/Semantic Warning/)
    end
    it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code" do
      fname = "#{TEST_QP_PATH}/min.qpl"
      begin
        File.delete("#{TEST_QP_PATH}/min.qpo")
      rescue
      end
      @cmp.compile fname
      expect(@cmp.failed).to be_false
      expect(@cmp.failure_message).to eq("")
      @cmp.write_qpo_file
      expect(File.exist?("#{TEST_QP_PATH}/min.qpo")).to be_true
      File.open("#{TEST_QP_PATH}/min.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{TEST_QP_PATH}/min.qpo") do |new_compile|
          new_data = new_compile.read
          expect(new_data).to eq(ref_data)
        end
      end
    end
    
    it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code when using compile_and_write_qpo" do
      fname = "#{TEST_QP_PATH}/min.qpl"
      begin
        File.delete("#{TEST_QP_PATH}/min.qpo")
      rescue
      end
      @cmp.compile_and_write_qpo fname
      expect(@cmp.failed).to be_false
      expect(@cmp.failure_message).to eq("")
      expect(File.exist?("#{TEST_QP_PATH}/min.qpo")).to be_true
      File.open("#{TEST_QP_PATH}/min.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{TEST_QP_PATH}/min.qpo") do |new_compile|
          new_data = new_compile.read
          expect(new_data).to eq(ref_data)
        end
      end
    end
    it "writes a .qpo file with the same name as the original .qpl file when imports are involved" do
      fname = "#{TEST_QP_PATH}/importer.qpl"
      begin
        File.delete("#{TEST_QP_PATH}/importer.qpo")
      rescue
      end
      @cmp.compile fname
      @cmp.write_qpo_file
      expect(File.exist?("#{TEST_QP_PATH}/importer.qpo")).to be_true
      File.open("#{TEST_QP_PATH}/importer.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{TEST_QP_PATH}/importer.qpo") do |new_compile|
          new_data = new_compile.read
          expect(new_data).to eq(ref_data)
        end
      end
    end
    it "sets failed to true if the desired qpl file is not existant" do
      @cmp.send_included_file "<getFirst>GarbageFileThatDoesNotExist</getFirst>"
      expect(@cmp.failed?).to be_true
    end
    it "sets failure message to the name of the file if the desired qpl file is not existant" do
      @cmp.send_included_file "<getFirst>GarbageFileThatDoesNotExist</getFirst>"
      expect(@cmp.failure_message).to match(/GarbageFileThatDoesNotExist/)
    end
    it "sets the success or fail message to '...successful...' if failed is false, and adds any failure message" do
      @cmp.failed = false
      @cmp.failure_message="testing"
      expect(@cmp.success_or_fail_message("file_name")).to eq("Compile of file_name was successful\ntesting")
    end
    it "sets the success or fail message to '...unsuccessful...' if failed is true, and adds any failure message" do
      @cmp.failed = true
      @cmp.failure_message="testing"
      expect(@cmp.success_or_fail_message("file_name")).to eq("Compile of file_name was unsuccessful\ntesting")
    end
  end
end