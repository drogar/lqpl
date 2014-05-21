# Encoding: UTF-8

require 'spec/spec_helper'

describe CodePointer do
  describe 'creation' do
    it 'should throw an exception with bad input' do
      expect { CodePointer.new('junk') }.to raise_error JSON::ParserError, /junk/
    end
    it 'should throw an exception with nil input' do
      expect { CodePointer.new(nil) }.to raise_error ModelCreateError, /not/
    end
    it 'should create a bare pointer with input ''' do
      cp = CodePointer.new('')
      expect(cp.qpo_method).to eq('')
      expect(cp.line_number).to eq(0)
    end
    it 'should create a CodePointer instance with correct input ' do
      @cp = CodePointer.new('{"codepointer": ["main", 0]}')
      expect(@cp.qpo_method).to eq(:main)
      expect(@cp.line_number).to eq(0)
    end
  end

  describe 'instance method normalize' do
    subject { CodePointer.new('{"codepointer": ["main", 17]}') }
    it 'should change the line_number to zero when input 1' do
      subject.normalize(1)
      expect(subject.line_number).to eq(0)
    end
    it 'should change the line_number to 5 when input 6' do
      subject.normalize(6)
      expect(subject.line_number).to eq(5)
    end
    it 'should leave the line_number at 17 when input 18 or more' do
      subject.normalize(18)
      expect(subject.line_number).to eq(17)
      subject.normalize(24)
      expect(subject.line_number).to eq(17)
    end

    it 'should change the line_number to 0 when input a negative number' do
      subject.normalize(-5)
      expect(subject.line_number).to eq(0)
    end
    it 'should change the line_number to 0 when input nil' do
      subject.normalize(nil)
      expect(subject.line_number).to eq(0)
    end
    it 'should change the line_number to 0 when input zero' do
      subject.normalize(0)
      expect(subject.line_number).to eq(0)
    end
  end
  describe 'mangle_to_selection_key' do
    subject { CodePointer.new('{"codepointer": ["main", 17]}') }
    it 'should return <methodname>--<linenumber>' do
      expect(subject.mangle_to_selection_key).to eq('main--17')
    end
  end
end
