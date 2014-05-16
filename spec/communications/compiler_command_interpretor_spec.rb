# encoding: utf-8
require 'spec/spec_helper'

json_version = '{"version_number" : ["0", "8", "4"], "version_string" :  ["90", "98", "94"]}'
program_one = "{\"file_name\":\"f\",\"qpl_program\":[\"qdata C = {H|T}\",\"app::(| ; )= {skip}\"]}"
illegal_input = '{"illegal_input" :"badInput"}'
describe CompilerCommandInterpretor do
  before :each do
    @cmp = double
  end
  subject { CompilerCommandInterpretor.new(@cmp) }
  context 'helper functions' do
    context 'make_version_number' do
      it 'takes the input  and returns 0.8.4' do
        res = CompilerCommandInterpretor.make_version_number(json_version)
        expect(res).to eq('0.8.4')
      end
    end
    describe 'current_version_line' do
      it 'sends a version request and returns a string' do
        expect(@cmp).to receive(:send_and_read_data).with('{"command" : "send_version"}')
          .and_return(json_version)
        expect(subject.current_version_line).to eql('Compiler: Version=0.8.4')
      end
    end
  end
  describe 'success_or_fail_message' do
    before(:each) do
      subject.qpl_file_name = 'whatever/this.qpl'
    end
    it 'says successful if no failure message' do
      subject.failure_message = nil
      expect(subject.success_or_fail_message).to match(/of this was success/)
    end
    it 'says unsuccessful and appends the failure message when there is one' do
      subject.warning('some')
      msg = subject.success_or_fail_message.lines.to_a
      expect(msg[0]).to match(/of this was unsuc/)
      expect(msg[1]).to match(/some/)
    end
  end
  context 'setters' do
    describe 'dir=' do
      it 'trims down to the directory of the file' do
        subject.dir = 'whatever/something/w.q'
        expect(subject.dir).to eql('whatever/something')
      end
    end
    describe 'qpo_file_name=' do
      it 'replaces qpl with qpo' do
        subject.qpo_file_name = 'whatsoev.qpler/some/this.qpl'
        expect(subject.qpo_file_name).to eql('whatsoev.qpler/some/this.qpo')
      end
    end

    describe 'qpl_file_name=' do
      it 'saves just the base name' do
        subject.qpl_file_name = '/whatsoev.qpler/some/this.qpl'
        expect(subject.qpl_file_name).to eql('this')
      end
    end
  end
  context 'compile' do
    context :read_qpl_file do
      it 'reads in a file' do
        expect(File).to receive(:readlines).with('f')
        subject.read_qpl_file('f')
      end
      it 'returns a json of the file' do
        expect(File).to receive(:readlines).with('f')
          .and_return(['qdata C = {H|T}', 'app::(| ; )= {skip}'])
        expect(subject.read_qpl_file('f')).to eql(program_one)
      end
    end
    it 'sets the directory and qpo file, reads the file' do
      expect(subject).to receive(:dir=).with('f')
      expect(subject).to receive(:qpo_file_name=).with('f')
      expect(subject).to receive(:send_file).with('f')
      allow(subject).to receive(:converse)
      subject.compile('f')
    end
    it 'sends the file data as json and then enters a communications loop' do
      allow(subject).to receive(:dir=).with('f')
      allow(subject).to receive(:qpo_file_name=).with('f')
      expect(subject).to receive(:read_qpl_file).with('f').and_return(program_one)
      expect(@cmp).to receive(:send_and_read_data).with(program_one).and_return('stuff')
      expect(subject).to receive(:converse).with('stuff')
      subject.compile('f')
    end
    it 'sets the failure message to nil if compiled without warnings' do
      subject.failure_message = 'old message'
      allow(subject).to receive(:dir=).with('f')
      allow(subject).to receive(:qpo_file_name=).with('f')
      allow(subject).to receive(:send_file).with('f')
      allow(subject).to receive(:converse)
      subject.compile('f')
      expect(subject.failure_message).to be_nil
    end

    it 'leaves the failure message if compiled with errors or warnings warnings' do
      subject.failure_message = 'old message'
      allow(subject).to receive(:dir=).with('f')
      allow(subject).to receive(:qpo_file_name=).with('f')
      allow(subject).to receive(:send_file).with('f').and_return(illegal_input)
      subject.compile('f')
      expect(subject.failure_message).to match(/^Illegal/)
    end
  end
  context 'converse' do
    describe 'send_file' do
      it 'does nothing when the arg is nil' do
        expect(@cmp).to_not receive(:send_data)
        subject.send_file(nil)
      end
      it 'reads the qpl file and sends on the connection' do
        expect(subject).to receive(:read_qpl_file).with('f').and_return('stuff')
        expect(@cmp).to receive(:send_and_read_data).with('stuff')
        subject.send_file('f')
      end
      it 'returns the result of the send' do
        allow(subject).to receive(:read_qpl_file)
        expect(@cmp).to receive(:send_and_read_data).and_return('stuff')
        expect(subject.send_file('f')).to eql('stuff')
      end
    end
    describe 'failure' do
      it 'should set the failure_message' do
        subject.failure('Warning', 'whatever')
        expect(subject.failure_message).to eql('Warning: whatever')
      end
      it 'should return nil' do
        expect(subject.failure('d', 'w')).to be_nil
      end
      context 'helpers' do
        before(:each) do
          expect(subject).to receive(:failure).and_call_original
        end
        specify do
          subject.warning('w')
          expect(subject.failure_message).to eql('Warning: w')
        end
        specify do
          subject.compile_fail('w')
          expect(subject.failure_message).to eql('Compile Failure: w')
        end
        specify do
          subject.illegal_input('w')
          expect(subject.failure_message).to eql('Illegal Input: w')
        end
      end
    end
    describe 'qpo' do
      it 'writes the data to a qpo file' do
        subject.dir = 'whatever/some.qpl'
        subject.qpo_file_name = 'whatever/some.qpl'
        expect(subject).to receive(:current_version_line).and_return('cv1')
        expect(File).to receive(:write).with('whatever/some.qpo', "cv1\nline1\nline2")
        subject.qpo(%w(line1 line2))
      end
      it 'returns nil' do
        allow(subject).to receive(:current_version_line).and_return('cv1')
        allow(File).to receive(:write).and_return(10)
        expect(subject.qpo(%w(line1 line2))).to be_nil
      end
    end
    it 'parses the parameter as JSON' do
      expect(JSON).to receive(:parse).with('f', symbolize_names: true)
        .and_return(JSON.parse(illegal_input, symbolize_names: true))
      subject.converse('f')
    end
    it 'exits if the parameter is nil without any activity' do
      expect(subject).to_not receive(:send)
      subject.converse(nil)
    end

    it 'uses the hash keys as methods to call' do
      expect(subject).to receive(:send_file).with('f')
      expect(subject).to receive(:qpo).with('what')
      expect(subject).to receive(:junk).with('junk')
      expect(subject).to_not receive(:warning)
      subject.converse('{"send_file":"f", "qpo":"what", "junk":"junk"}')
    end
    it 'loops when more files are needed' do
      expect(JSON).to receive(:parse).exactly(2).times.and_call_original
      expect(subject).to receive(:send_file).and_return('{"warning":"warned"}')
      expect(subject).to receive(:warning).and_call_original
      subject.converse('{"send_file":"f"}')
    end
  end
end
