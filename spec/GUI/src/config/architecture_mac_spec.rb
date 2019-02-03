require 'architecture_mac'

describe ArchitectureMac do
  it 'is valid' do
    expect(ArchitectureMac).not_to be_nil
  end
  context 'architecture checks' do
    subject { ArchitectureMac.new }
    it 'checks for a Mac' do
      expect(subject.mac?).to be true
    end
    it 'checks for Linux' do
      expect(subject.linux?).to be false
    end
    it 'checks for Linux' do
      expect(subject.windows?).to be false
    end
  end
end
