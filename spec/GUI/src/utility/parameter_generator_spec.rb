
describe ParameterGenerator do
  context 'no parameters' do
    subject { ParameterGenerator.new }
    specify { expect(subject.parameters_for_calling).to eql('') }
    specify { expect(subject.parameters_for_definition).to eql('') }
  end
  context 'With parameters' do
    subject { ParameterGenerator.new([1, 4, 79]) }
    specify { expect(subject.parameters_for_calling).to eql('def1, def2, def3') }
    specify { expect(subject.parameters_for_definition).to eql('(def1 = 1, def2 = 4, def3 = 79)') }
  end
end
