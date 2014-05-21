# Encoding: UTF-8
require 'spec/spec_helper'

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
end
