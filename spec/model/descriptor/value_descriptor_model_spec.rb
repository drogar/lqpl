# Encoding: UTF-8
require 'spec/spec_helper'

require 'GUI/src/panels/quantum_stack/quantum_stack_model'

describe ValueDescriptorModel do
  it 'should successfully be created with input {value:0.5}' do
    z = ValueDescriptorModel.new '{"value":0.5}'
    expect(z).not_to be_nil
  end
  it 'should raise an error on other input' do
    expect do
      ValueDescriptorModel.new '{"value":[4,5]}'
    end.to raise_error ModelCreateError, /value/
    expect do
      ValueDescriptorModel.new 'whatever'
    end.to raise_error JSON::ParserError, /whatever/
  end
  context 'class methods' do
    context 'validation' do
      it 'should raise an error if passed an array with elements' do
        expect do
          ValueDescriptorModel.validate_substacks_count([1, 2])
        end.to raise_error ModelCreateError, /Value.*should not have/
      end
      it 'should not raise an error if passed an empty array' do
        expect(ValueDescriptorModel.validate_substacks_count([])).to be_nil
      end
      it 'should not raise an error if passed an array with no elements' do
        expect(ValueDescriptorModel.validate_substacks_count(nil)).to be_nil
      end
    end
  end
  context 'attributes' do
    it 'should always have a length of 0' do
      sd = AbstractDescriptorModel.make_instance '{"value":0.5}'
      expect(sd.length).to eql(0)
    end
    it 'should have the value in the construction string' do
      sd = AbstractDescriptorModel.make_instance '{"value":6.25e-2}'
      expect(sd.value).to eq(6.25e-2)
    end
    it 'should have no name' do
      sd = AbstractDescriptorModel.make_instance '{"value":0.5}'
      expect(sd.name).to be_nil
    end
    it 'should return nil when asked for substack labels' do
      sd = AbstractDescriptorModel.make_instance '{"value":0.5}'
      expect(sd.substack_labels).to be_nil
    end
  end
end
