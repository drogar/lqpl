# Encoding: UTF-8
require 'spec/spec_helper'

require 'GUI/src/panels/quantum_stack/quantum_stack_model'

QUBIT_ZZ = '{"qubit":["ZZ"]}'
QUBIT_ZZOO = '{"qubit":["ZZ", "OO"]}'
QUBIT_ZZZOOZOO = '{"qubit":["ZZ", "ZO", "OZ", "OO"]}'
describe QubitDescriptorModel do
  it 'should raise an error if constructed with  other than a list of Z,O pairs' do
    expect do
      AbstractDescriptorModel.make_instance '{"qubit":["ZP"]}'
    end.to raise_error(ModelCreateError, /qubit....ZP/)
  end
  it 'should have a length equal to the number of elements of the passed in list' do
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZ
    expect(sd.length).to eql(1)
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZOO
    expect(sd.length).to eql(2)
  end
  it 'should have the value being the list of qubit indicators in the string' do
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZZOOZOO
    expect(sd.value).to eq([[0, 0], [0, 1], [1, 0], [1, 1]])
  end
  it 'should return a list of length length when asked for substack labels' do
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZ
    expect(sd.length).to eql(1)
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZOO
    expect(sd.length).to eql(2)
  end
  it 'should have the substack_labels = list of 01 pairs in the construction string' do
    sd = AbstractDescriptorModel.make_instance QUBIT_ZZZOOZOO
    expect(sd.substack_labels).to eq(%w(00 01 10 11))
  end
  context 'class methods' do
    context 'validation' do
      it 'should not raise an error if passed an array with 1 element' do
        expect(QubitDescriptorModel.validate_substacks_count([1])).to be_nil
      end
      it 'should not raise an error if passed an array with 2 elements' do
        expect(QubitDescriptorModel.validate_substacks_count([1, 2])).to be_nil
      end
      it 'should not raise an error if passed an array with 3 elements' do
        expect(QubitDescriptorModel.validate_substacks_count([1, 2, 3])).to be_nil
      end
      it 'should not raise an error if passed an array with 4 elements' do
        expect(QubitDescriptorModel.validate_substacks_count([1, 2, 3, 4])).to be_nil
      end
      it 'should raise an error if passed an empty array' do
        expect do
          QubitDescriptorModel.validate_substacks_count([])
        end.to raise_error ModelCreateError, /Qubit.*must have/
      end
      it 'should raise an error if passed a nil array' do
        expect do
          QubitDescriptorModel.validate_substacks_count(nil)
        end.to raise_error ModelCreateError, /Qubit.*must have/
      end
      it 'should raise an error if passed an array with > 4 elements' do
        expect do
          QubitDescriptorModel.validate_substacks_count([1, 2, 3, 4, 5])
        end.to raise_error ModelCreateError, /Qubit.*must have/
      end
    end
  end
end
