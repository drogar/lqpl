describe LqplController do
  before :each do
    SwingRunner.on_edt do
      @l = LqplController.instance
    end
  end
  describe 'on create' do
    specify { expect(LqplController::SUBS.size).to eq(5) }
    specify { expect(LqplController::DIALOGS.size).to eq(2) }
  end
  describe 'sub handlers' do
    let(:dh) { double('dh') }
    let(:sh) { double('sh') }
    subject { @l }
    before :each do
      subject.sub_controllers_handler = sh
      subject.dialogs_handler = dh
      allow(dh).to receive(:dispose_all)
      allow(sh).to receive(:dispose_all)
    end
    after :each do
      subject.sub_controllers_handler = nil
      subject.dialogs_handler = nil
    end
    describe 'close' do
      it 'should send "dispose_all" to each of the sub handlers' do
        expect(dh).to receive(:dispose_all)
        expect(sh).to receive(:dispose_all)
        allow(ExitHandler.instance).to receive(:close_servers)
        subject.close
      end
    end

    describe 'update_all' do
      it 'should call update_all on the subs' do
        expect(dh).to_not receive(:update_all)
        expect(sh).to receive(:update_all)
        subject.update_all
      end
    end
  end
  describe :help_about_action_performed do
    it 'gets the AboutController instance and passes nil to the handleAbout method' do
      ac = double('aboutController')
      allow(AboutController).to receive(:instance) { ac }
      allow(ac).to receive(:handleAbout)
      @l.help_about_action_performed
      expect(AboutController).to have_received(:instance)
      expect(ac).to have_received(:handleAbout).with(nil)
    end
  end
  describe :file_compile_action_performed do
    before(:each) do
      allow(@l).to receive(:update_view)
      allow(LqplFileChooser).to receive(:open_and_compile)
    end
    it 'calls LqplFileChooser.open_and_compile with my frame and model' do
      @l.file_compile_action_performed
      expect(LqplFileChooser).to have_received(:open_and_compile).with(@l.my_frame, a_kind_of(LqplModel))
    end
    it 'calls update_view' do
      @l.file_compile_action_performed
      expect(@l).to have_received(:update_view)
    end
  end
  describe :file_load_action_performed do
    before(:each) do
      allow(@l).to receive(:update_view)
      allow(LqplFileChooser).to receive(:open_and_load_qpl) { false }
    end
    it 'calls LqplFileChooser.open_and_load_qpl with my model' do
      @l.file_load_action_performed
      expect(LqplFileChooser).to have_received(:open_and_load_qpl).with(a_kind_of(LqplModel))
    end
    it 'calls update_view' do
      @l.file_load_action_performed
      expect(@l).to have_received(:update_view)
    end
    it 'calls initialize_sub_controllers when the load returns true' do
      allow(@l).to receive(:initialize_sub_controllers)
      allow(LqplFileChooser).to receive(:open_and_load_qpl) { true }
      @l.file_load_action_performed
      expect(@l).to have_received(:initialize_sub_controllers)
    end
  end
  describe :file_simulate_action_performed do
    let(:src) { double('SimulateResultsController') }
    let(:stc) { double('StackTranslationController') }
    before(:each) do
      allow(SimulateResultsController).to receive(:instance) { src }
      allow(StackTranslationController).to receive(:instance) { stc }
      allow(src).to receive(:set_simulate_results)
      allow(src).to receive(:open)
      allow(stc).to receive(:stack_translation) { :stack_t }
    end
    it 'sets the simulate results based on recursion depth and the stack translation' do
      @l.file_simulate_action_performed
      expect(SimulateResultsController).to have_received(:instance).twice
      expect(src).to have_received(:set_simulate_results).with(@l.recursion_depth, :stack_t)
    end
    it 'opens the simulate results controller' do
      @l.file_simulate_action_performed
      expect(src).to have_received(:open)
    end
  end
end
