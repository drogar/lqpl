# Encoding: UTF-8
require 'spec/spec_helper'

require 'GUI/src/panels/quantum_stack/quantum_stack_model'

C14 = '{"classical": [14]}'
C1420 =  '{"classical": [14,20,true]}'
describe ClassicalDescriptorModel do
  it 'should raise an error if passed other than a list of ints/bools' do
    expect do
      AbstractDescriptorModel.make_instance '{"classical": ["wjat"]}'
    end.to raise_error(ModelCreateError, /classical.....wjat/)
  end
  it 'should have a length equal to the number of elements of the passed in list' do
    sd = AbstractDescriptorModel.make_instance C14
    expect(sd.length).to eql(1)
    sd = AbstractDescriptorModel.make_instance C1420
    expect(sd.length).to eql(3)
  end
  it 'should have the value being the list of classicalvalues in the construction string' do
    sd = AbstractDescriptorModel.make_instance C1420
    expect(sd.value).to eq([14, 20, true])
  end
  it 'should return a list of length "length" when asked for substack labels' do
    sd = AbstractDescriptorModel.make_instance C14
    expect(sd.length).to eql(1)
    sd = AbstractDescriptorModel.make_instance C1420
    expect(sd.length).to eql(3)
  end
  it 'should return substack_labels = to_s of of its classicalvalues' do
    sd = AbstractDescriptorModel.make_instance C1420
    expect(sd.substack_labels).to eq(%w(14 20 true))
  end
  context 'class methods' do
    context 'validation' do
      it 'should not raise an error if passed an array with elements' do
        expect(ClassicalDescriptorModel.validate_substacks_count([1, 2])).to be_nil
      end
      it 'should raise an error if passed an empty array' do
        expect do
          ClassicalDescriptorModel.validate_substacks_count([])
        end.to raise_error ModelCreateError, /Classical.*should have/
      end
      it 'should raise an error if passed a nil array' do
        expect do
          ClassicalDescriptorModel.validate_substacks_count(nil)
        end.to raise_error ModelCreateError, /Classical.*should have/
      end
    end
  end
end
