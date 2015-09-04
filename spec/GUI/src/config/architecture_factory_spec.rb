# encoding: utf-8
require 'architecture_factory'

describe ArchitectureFactory do
  subject { ArchitectureFactory.architecture_category(config_map) }
  context 'host_os is darwin' do
    let(:config_map) { { 'host_os' => 'darwin' } }
    it 'is a mac' do
      expect(subject.mac?).to be true
    end
  end
  context 'host_os is darwin' do
    let(:config_map) { { 'host_os' => 'linux' } }
    it 'is Linux' do
      expect(subject.linux?).to be true
    end
  end
  context 'host_os is darwin' do
    let(:config_map) { { 'host_os' => 'win' } }
    it 'is windows' do
      expect(subject.windows?).to be true
    end
  end
end
