require 'emulator_command_interpretor'

describe EmulatorCommandInterpretor do
  before :each do
    @cmp = double
  end
  subject { EmulatorCommandInterpretor.new(@cmp) }
  specify { expect(subject).to be_a(EmulatorCommandInterpretor) }
  describe :send_load_from_file do
    it 'reads in a file and sends and reads a load command' do
      expect(File).to receive(:readlines).with('somefile')
        .and_return(%w(abc 123))
      expect(@cmp).to receive(:send_and_read_data)
        .with('{"load_entry":1,"load_lines":["abc","123"]}')
      subject.send_load_from_file(1, 'somefile')
    end
  end
  describe 'commands' do
    describe :command do
      it 'sends and receive with command == 1st parameter,  parameters = [1, 2]' do
        expect(@cmp).to receive(:send_and_read_data).with('{"command":"w","parameters":[1,2]}')
        subject.command(:w, [1, 2])
      end
      it 'sends and receive with command == 1st parameter,  parameters = [1]' do
        expect(@cmp).to receive(:send_and_read_data).with('{"command":"w","parameters":[1]}')
        subject.command(:w, [1])
      end
      it 'sends and receive with command == 1st parameter,  parameters = []' do
        expect(@cmp).to receive(:send_and_read_data).with('{"command":"wat","parameters":[]}')
        subject.command(:wat, [])
      end
    end
    describe :get_qstack do
      it 'calls command with qstack,  5, 1,  when called with no parms' do
        expect(subject).to receive(:command).with(:get_qstack, [5, 1])
        subject.get_qstack
      end
      it 'calls get command with qstack 4, 1 if passed a single 4' do
        expect(subject).to receive(:command).with(:get_qstack,  [4, 1])
        subject.get_qstack(4)
      end
      it 'calls get command with qstack 4, 3 if passed a 4 3' do
        expect(subject).to receive(:command).with(:get_qstack, [4, 3])
        subject.get_qstack(4, 3)
      end
    end
    describe :do_trim do
      it 'calls command with :trim and []' do
        expect(subject).to receive(:command).with(:trim, [])
        subject.do_trim
      end
    end
    describe :do_run do
      it 'calls command with :run and [1] when called with no parms' do
        expect(subject).to receive(:command).with(:run, [1])
        subject.do_run
      end
      it 'falls through to connection with :run and [1] when called with no parms' do
        expect(@cmp).to receive(:send_and_read_data).with('{"command":"run","parameters":[1]}')
        subject.do_run
      end
      it 'calls command with :run and [4] when called with 4' do
        expect(subject).to receive(:command).with(:run, [4])
        subject.do_run(4)
      end
    end
  end

  describe 'generated methods' do
    specify { expect(subject.methods).to include(:get_qstack) }
    specify { expect(subject.methods).to include(:get_stack_translation) }
    specify { expect(subject.methods).to include(:get_classical_stack) }
    specify { expect(subject.methods).to include(:get_dump) }
    specify { expect(subject.methods).to include(:get_code) }
    specify { expect(subject.methods).to include(:get_codepointer) }
    specify { expect(subject.methods).to include(:do_run) }
    specify { expect(subject.methods).to include(:do_trim) }
    specify { expect(subject.methods).to include(:do_simulate) }
    specify { expect(subject.methods).to include(:do_depth_multiple) }
  end
end
