# encoding: utf-8
require 'spec/spec_helper'

describe CompilerCommandInterpretor do
  context "helper functions" do
    context "make_version_number" do
      it "takes the input CS_VERSION [0,8,4] [] and returns 0.8.4" do
        res = CompilerCommandInterpretor.make_version_number("CS_VERSION [0,8,4] []")
        expect(res).to eq("0.8.4")
      end
    end
  end

  let(:conn) {double('connection')}
  subject {CompilerCommandInterpretor.new(conn)}
  specify {expect(CompilerCommandInterpretor::COMMAND_START).to eq(/(CS_)|(<qpo)|(<compilefail)|(<getFirst)/)}
  specify {expect(subject.add_non_command_line('something')).to eq('something')}
  specify {expect(subject.add_non_command_line('CS_')).to eq('')}
  specify {expect(subject.add_non_command_line(' kljw  <qpo')).to eq('')}


  context "interfaces with the lqpl-compiler-server" do
    before :each do
      @cmp = CompilerServerConnection.get_instance
      @cmp.connect
    end
    after(:all) do
      @cmp.close_down if @cmp
    end
    subject {CompilerCommandInterpretor.new(@cmp)}
    it "sends QPL code to the lqpl-compiler-server and gets qpo code back" do
      fname = "#{TEST_QP_PATH}/min.qpl"
      qpocode = subject.compile fname
      expect(qpocode).to match(/app_fcdlbl.*/)
    end
    it "signals failure when the code has a syntax error" do
      subject.compile "#{TEST_QP_PATH}/invalidsyntax.qpl"
      expect(subject.failed?).to be true
      expect(subject.failure_message).to match(/unexpected/)
    end
    it "signals failure when the code has a semantic error" do
      subject.compile "#{TEST_QP_PATH}/invalidsemantics.qpl"
      expect(subject.failed?).to be true
      expect(subject.failure_message).to match(/Semantic Error/)
    end
    it "signals a warning when the code has a balance creation error" do
      fname = "#{TEST_QP_PATH}/invalidbalance.qpl"
      subject.compile fname
      expect(subject.failed?).to be false
      expect(subject.failure_message).to match(/Semantic Warning/)
    end
    it "writes a .qpo file with the same name as the original .qpl file with the corresponding QPO code" do
      fname = "#{TEST_QP_PATH}/min.qpl"
      begin
        File.delete("#{TEST_QP_PATH}/min.qpo")
      rescue
      end
      subject.compile fname
      expect(subject.failed?).to be false
      expect(subject.failure_message).to eq("")
      expect(File.exist?("#{TEST_QP_PATH}/min.qpo")).to be true
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
      subject.compile fname
      expect(File.exist?("#{TEST_QP_PATH}/importer.qpo")).to be true
      File.open("#{TEST_QP_PATH}/importer.reference.qpo") do |ref_compile|
        ref_data = ref_compile.read
        File.open("#{TEST_QP_PATH}/importer.qpo") do |new_compile|
          new_data = new_compile.read
          expect(new_data).to eq(ref_data)
        end
      end
    end
    it "sets failed to true if the desired qpl file is not existant" do
      subject.send_included_file "<getFirst>GarbageFileThatDoesNotExist</getFirst>"
      expect(subject.failed?).to be true
    end
    it "sets failure message to the name of the file if the desired qpl file is not existant" do
      subject.send_included_file "<getFirst>GarbageFileThatDoesNotExist</getFirst>"
      expect(subject.failure_message).to match(/GarbageFileThatDoesNotExist/)
    end
    it "sets the success or fail message to '...successful...' if failed is false, and adds any failure message" do
      subject.failed = false
      subject.failure_message="testing"
      expect(subject.success_or_fail_message("file_name")).to eq("Compile of file_name was successful\ntesting")
    end
    it "sets the success or fail message to '...unsuccessful...' if failed is true, and adds any failure message" do
      subject.failed = true
      subject.failure_message="testing"
      expect(subject.success_or_fail_message("file_name")).to eq("Compile of file_name was unsuccessful\ntesting")
    end
  end
end