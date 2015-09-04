# encoding: utf-8
require 'testing_fest'

describe TestingFest do
  it 'is valid' do
    expect(TestingFest).not_to be_nil
  end
  context 'when property is not set' do
    subject do
      java.lang.System.clear_property('com.drogar.testing.fest')
      TestingFest.new
    end
    it 'returns false for testing' do
      expect(subject.testing?).to be false
    end
    it 'returns true for not_testing' do
      expect(subject.not_testing?).to be true
    end
  end
  context 'when property is set, but not to "true"' do
    subject do
      java.lang.System.set_property('com.drogar.testing.fest', 'whatever')
      TestingFest.new
    end
    it 'returns false for testing' do
      expect(subject.testing?).to be false
    end
    it 'returns true for not_testing' do
      expect(subject.not_testing?).to be true
    end
  end
  context 'when property is set to "true"' do
    subject do
      java.lang.System.set_property('com.drogar.testing.fest', 'true')
      TestingFest.new
    end
    it 'returns true for testing' do
      expect(subject.testing?).to be true
    end
    it 'returns false for not_testing' do
      expect(subject.not_testing?).to be false
    end
  end
end
