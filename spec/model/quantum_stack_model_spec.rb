# Encoding: UTF-8
require 'spec/spec_helper'

require 'spec/specdata/quantum_stack_data'
require 'GUI/src/panels/quantum_stack/quantum_stack_model'

describe QuantumStackModel do
  describe 'instance setup' do
    subject { QuantumStackModel.new }
    before :each do
      subject.stack_translation = double('StackTranslation', :reverse_lookup => 'p', :nil? => false)
    end
    it 'should give an invalid create error when created with incorrect data' do
      expect { subject.quantum_stack = 'err' }.to raise_error JSON::ParserError, /err/
    end
    it 'should give an error when stackzero has substacks' do
      expect do
        subject.quantum_stack = bottom_stack('1', 'true', NODE_ZERO)
      end.to raise_error ModelCreateError, /not have/
    end
    it 'should give an error when stackvalue has substacks' do
      expect do
        subject.quantum_stack = bottom_stack('1', 'true', NODE_VAL5)
      end.to raise_error ModelCreateError, /not have/
    end
    it 'should give an error when stackqubit does not have substacks' do
      expect do
        subject.quantum_stack = make_multi_sstacks('1', 'true', NODE_QZZ, [])
      end.to raise_error ModelCreateError, /must have/
    end
    it 'should give an error when stackclassical does not have substacks'  do
      expect do
        subject.quantum_stack = make_multi_sstacks('1', 'true', NODE_CL275, [])
      end.to raise_error ModelCreateError, /should have/
    end
    it 'should give an error when stackdata does not have substacks' do
      expect do
        subject.quantum_stack = make_multi_sstacks('1', 'true', NODE_DATA, [])
      end.to raise_error ModelCreateError, /should have/
    end
    it 'should successfully create the start qstack' do
      subject.quantum_stack = make_multi_sstacks('-1', 'true', NODE_VAL1, [])
      expect(subject).not_to be_bottom
    end
    it 'should allow bottom as the construction'  do
      subject.quantum_stack = '{"bottom": true}'
      expect(subject).to be_bottom
    end
    it 'should allow bottom in place of substacks'  do
      subject.quantum_stack = bottom_stack('1', 'true', NODE_QZZ)
    end
    it 'should have the same number of substacks as the length of the descriptor' do
      subject.quantum_stack = bottom_stack('1', 'true', NODE_QZZ)
      expect(subject.substacks.size).to eql(1)
      subject.quantum_stack = bottom_stack('1', 'true', NODE_CL2757, 3)
      expect(subject.substacks.size).to eql(3)
    end
    describe 'instance method make_name' do
      before :each do
        st = double('StackTranslation', :nil? => false)
        allow(st).to receive(:reverse_lookup) do |val|
          case val
          when 1 then '@q'
          when 2 then '@r'
          when -1 then '-1'
          else "#{@val}"
          end
        end
        subject.stack_translation = st
      end
      it 'returns the stackaddress as a string if not found in the translation' do
        subject.quantum_stack = make_multi_sstacks('-1', 'true', NODE_VAL1, [])
        expect(subject.make_name(:use_stack_address)).to eq('')
        expect(subject.make_name(:hide_stack_address)).to eq('')
      end
      it 'returns the stackaddress found in the translation with use_stack_address symbol passed' do
        subject.quantum_stack = make_multi_sstacks('1', 'true', NODE_VAL1, [])
        expect(subject.make_name(:use_stack_address)).to eq('@q(1)')
      end
      it 'gives the stackaddress found in the translation with hide_stack_address symbol passed' do
        subject.quantum_stack = make_multi_sstacks('1', 'true', NODE_VAL1, [])
        expect(subject.make_name(:hide_stack_address)).to eq('@q')
      end
    end
  end
  it 'should raise an exception if the stack is assigned before the translation' do
    expect do
      qs = QuantumStackModel.new
      qs.quantum_stack = bottom_stack('1', 'true', NODE_ZERO)
    end.to raise_error ModelCreateError, /Missing/
  end
  it 'should assign a name for a quantum descriptor' do
    qs = QuantumStackModel.new
    st = double('StackTranslation', :nil? => false)
    expect(st).to receive(:reverse_lookup).and_return('p')
    qs.stack_translation = st
    qs.quantum_stack = QB2WITHBOTTOM
    expect(qs.descriptor.name).to eq('p(2)')
  end
  it 'should assign names to multi-level qstacks' do
    qs = QuantumStackModel.new
    st = double('StackTranslation', :nil? => false)
    allow(st).to receive(:reverse_lookup) do |val|
      case val
      when 1 then '@q'
      when 2 then '@r'
      when -1 then '-1'
      end
    end
    qs.stack_translation = st
    qs.quantum_stack = QSQ1R2
    expect(qs.descriptor.name).to eq('@r(2)')
    expect(qs.substacks[0].descriptor.name).to eq('@q(1)')
  end
end
