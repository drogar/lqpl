require 'architecture_win'

describe ArchitectureWin do
  it 'is valid' do
    expect(ArchitectureWin).not_to be_nil
  end
  context 'architecture checks' do
    subject { ArchitectureWin.new }
    it 'checks for a Win' do
      expect(subject.windows?).to be true
    end
    it 'checks for Linux' do
      expect(subject.linux?).to be false
    end
    it 'checks for Mac' do
      expect(subject.mac?).to be false
    end
  end
end
