# Encoding: UTF-8
require 'spec/spec_helper'

require 'GUI/src/panels/quantum_stack/quantum_stack_model'

describe ZeroDescriptorModel do
  it 'Should create when given no input' do
    z = ZeroDescriptorModel.new
    expect(z).not_to be_nil
  end
  it 'should successfully be created with input {zero:0}' do
    z = ZeroDescriptorModel.new '{"zero":0}'
    expect(z).not_to be_nil
  end
  it 'should raise an error on other input' do
    expect do
      ZeroDescriptorModel.new '{"value":0.5}'
    end.to raise_error ModelCreateError, /value/
    expect do
      ZeroDescriptorModel.new 'whatever'
    end.to raise_error JSON::ParserError, /whatever/
  end

  context 'class methods' do
    context 'validation' do
      it 'should raise an error if passed an array with elements' do
        expect do
          ZeroDescriptorModel.validate_substacks_count([1, 2])
        end.to raise_error ModelCreateError, /Zero.*should not have/
      end
      it 'should not raise an error if passed an empty array' do
        expect(ZeroDescriptorModel.validate_substacks_count([])).to be_nil
      end
      it 'should not raise an error if passed an array with no elements' do
        expect(ZeroDescriptorModel.validate_substacks_count(nil)).to be_nil
      end
    end
  end
  context 'attributes' do
    subject { ZeroDescriptorModel.new }
    it 'should have a value of 0' do
      expect(subject.value).to eq('0')
    end
    it 'should have a length of 0' do
      expect(subject.length).to eq(0)
    end
    it 'should have no name' do
      expect(subject.name).to be_nil
    end
    it 'should return nil when asked for substack labels' do
      expect(subject.substack_labels).to be_nil
    end
  end
end
