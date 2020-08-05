require 'architecture_category'

describe ArchitectureCategory do
  it 'is valid' do
    expect(ArchitectureCategory).not_to be_nil
  end
  context 'architecture checks' do
    subject { ArchitectureCategory.new }
    it 'is false for a Mac' do
      expect(subject.mac?).to be false
    end
    it 'false for Linux' do
      expect(subject.linux?).to be false
    end
    it 'false for Linux' do
      expect(subject.windows?).to be false
    end
  end
end
