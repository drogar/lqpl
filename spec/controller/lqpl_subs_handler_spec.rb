require 'spec/spec_helper'

describe LqplSubsHandler do
  let(:c1) { double('c1') }
  let(:c2) { double('c2') }
  let(:s1) { double('sub1') }
  let(:s2) { double('sub2') }
  before(:each) do
    expect(c1).to receive(:instance).and_return(s1)
    expect(c2).to receive(:instance).and_return(s2)
  end
  subject { LqplSubsHandler.new([c1, c2]) }

  describe 'all_controllers_dispose' do
    it 'calls dispose on each held controller' do
      expect(s1).to receive(:dispose)
      expect(s2).to receive(:dispose)
      subject.dispose_all
    end
  end

  describe 'sub_controllers_open' do
    it "should send 'open' to each member of the subcontrollers" do
      expect(s1).to receive(:open)
      expect(s2).to receive(:open)
      subject.open
    end
  end

  describe 'update_all' do
    it "should send 'update_data_from_lqpl_model' to each member of the subcontrollers" do
      expect(s1).to receive(:update_data_from_lqpl_model)
      expect(s2).to receive(:update_data_from_lqpl_model)
      subject.update_all('')
    end
  end

  describe 'update_and_open' do
    it "should send 'update_data_from_lqpl_model' to each member of the subcontrollers" do
      expect(s1).to receive(:update_data_from_lqpl_model)
      expect(s2).to receive(:update_data_from_lqpl_model)
      expect(s1).to receive(:open)
      expect(s2).to receive(:open)
      subject.update_and_open('')
    end
  end

  describe 'update_on_trim' do
    it "should send 'update_data_from_lqpl_model' to sub1 and not to sub2" do
      expect(s1).to receive(:update_data_from_lqpl_model)
      expect(s1).to receive(:update_on_lqpl_model_trim).and_return(true)
      expect(s2).to receive(:update_on_lqpl_model_trim).and_return(false)
      expect(s2).not_to receive(:update_data_from_lqpl_model)
      subject.update_on_trim('')
    end
  end
end
