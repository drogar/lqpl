# Encoding: UTF-8

require 'spec/spec_helper'
require 'spec/specdata/stack_translation_data'
require 'GUI/src/panels/stack_translation/stack_translation_model'

describe StackTranslationModel do
  describe 'setup' do
    subject { StackTranslationModel.new }
    it 'should give an invalid create error when sent incorrect data' do
      expect { subject.stack_translation = 'err' }.to raise_error JSON::ParserError, /err/
    end
    it 'should create a text representation in its text attribute' do
      subject.stack_translation = P1
      expect(subject.text).to eq('<html><ol><li>p=>1</li></ol></html>')
      subject.stack_translation = L3STACK
      expect(subject.text).to eq('<html><ol><li>p=>1</li><li>p=>2</li><li>rex=>27, p=>3</li></ol></html>')
    end
    it 'should ignore direct assignment to the text attribute' do
      subject.stack_translation = P1
      subject.text = 'junk'
      expect(subject.text).to eq('<html><ol><li>p=>1</li></ol></html>')
    end
  end
  describe 'it should provide reverse lookups' do
    subject { StackTranslationModel.new }
    it 'should return the requested value if it is not found on reverse lookup' do
      expect(subject.reverse_lookup(15)).to eq('15')
    end
    it 'should return the name when there is only one entry' do
      subject.stack_translation = P1
      expect(subject.reverse_lookup(1)).to eq('p')
    end
    it 'should return the name of the first entry when there are repeated keys in multiple lists' do
      subject.stack_translation = P1ANDR1
      expect(subject.reverse_lookup(1)).to eq('p')
      subject.stack_translation = P1ANDR1ANDS1
      expect(subject.reverse_lookup(1)).to eq('p')
      subject.stack_translation = P1ANDEMPTYANDS1
      expect(subject.reverse_lookup(1)).to eq('p')
    end
    it 'should return the keys for all the values in the list of maps' do
      subject.stack_translation = L3STACK
      expect(subject.reverse_lookup(1)).to eq('p')
      expect(subject.reverse_lookup(2)).to eq('p')
      expect(subject.reverse_lookup(3)).to eq('p')
      expect(subject.reverse_lookup(27)).to eq('rex')
    end
  end
end
