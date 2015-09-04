# encoding: utf-8
require 'architecture_linux'

describe ArchitectureLinux do
  it 'is valid' do
    expect(ArchitectureLinux).not_to be_nil
  end
  context 'architecture checks' do
    subject { ArchitectureLinux.new }
    it 'checks for a windows' do
      expect(subject.windows?).to be false
    end
    it 'checks for Linux' do
      expect(subject.linux?).to be true
    end
    it 'checks for Mac' do
      expect(subject.mac?).to be false
    end
  end
end
