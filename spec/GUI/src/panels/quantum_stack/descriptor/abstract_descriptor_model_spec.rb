require 'quantum_stack_model'

describe AbstractDescriptorModel do
  subject { AbstractDescriptorModel }
  it 'should only be created by the factory' do
    expect { AbstractDescriptorModel.new }.to raise_error(ModelCreateError)
  end
  it 'has a value accessor' do
    expect(subject.instance_methods).to include(:value)
    expect(subject.instance_methods).to include(:value=)
  end
  it 'has a name accessor' do
    expect(subject.instance_methods).to include(:name)
    expect(subject.instance_methods).to include(:name=)
  end
  it 'has a substack_labels method' do
    expect(subject.instance_methods).to include(:substack_labels)
  end
  it 'has a scalar? method' do
    expect(subject.instance_methods).to include(:scalar?)
  end
end
