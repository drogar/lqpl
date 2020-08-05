require 'descriptor_model_factory'

describe DescriptorModelFactory do
  subject { DescriptorModelFactory }
  describe 'it creates on string input' do
    it 'should create an instance of ZeroDescriptorModel when created with  {zero:0}' do
      expect(ZeroDescriptorModel).to receive(:new).with(EnsureJSON.new('{"zero":0}').as_json)
      DescriptorModelFactory.make_model '{"zero":0}'
    end
    it 'should create an instance of ValueDescriptorModel when created with {val:0.5}' do
      expect(ValueDescriptorModel).to receive(:new).with(EnsureJSON.new('{"value":0.5}').as_json)
      DescriptorModelFactory.make_model '{"value":0.5}'
    end
    it 'should call ClassicalDescriptorModel new  when created with {classical: [..]}' do
      expect(ClassicalDescriptorModel).to receive(:new)
        .with(EnsureJSON.new('{"classical" : [1, true]}').as_json)
      DescriptorModelFactory.make_model('{"classical" : [1, true]}')
    end
    it 'should call QubitDescriptorModel new when created with {qubit : [...]}' do
      expect(QubitDescriptorModel).to receive(:new)
        .with(EnsureJSON.new('{"qubit" : ["ZZ", "ZO", "OZ", "OO"]}').as_json)
      DescriptorModelFactory.make_model('{"qubit" : ["ZZ", "ZO", "OZ", "OO"]}')
    end
    it 'should call DataDescriptorModel new when created with   "{\"data\" : [..]}' do
      expect(DataDescriptorModel).to receive(:new).with(EnsureJSON.new('{"data":[]}').as_json)
      DescriptorModelFactory.make_model('{"data":[]}')
    end
  end
  describe 'json input' do
    it 'should create an instance of ZeroDescriptorModel when created with  {zero:0}' do
      expect(ZeroDescriptorModel).to receive(:new).with(EnsureJSON.new('{"zero":0}').as_json)
      DescriptorModelFactory.make_model EnsureJSON.new('{"zero":0}').as_json
    end
    it 'should create an instance of ValueDescriptorModel when created with {val:0.5}' do
      expect(ValueDescriptorModel).to receive(:new).with(EnsureJSON.new('{"value":0.5}').as_json)
      DescriptorModelFactory.make_model EnsureJSON.new('{"value":0.5}').as_json
    end
    it 'should call ClassicalDescriptorModel new  when created with {classical: [..]}' do
      expect(ClassicalDescriptorModel).to receive(:new)
        .with(EnsureJSON.new('{"classical" : [1, true]}').as_json)
      DescriptorModelFactory.make_model(EnsureJSON.new('{"classical" : [1, true]}').as_json)
    end
    it 'should call QubitDescriptorModel new when created with {qubit : [...]}' do
      expect(QubitDescriptorModel).to receive(:new)
        .with(EnsureJSON.new('{"qubit" : ["ZZ", "ZO", "OZ", "OO"]}').as_json)
      DescriptorModelFactory
        .make_model(EnsureJSON.new('{"qubit" : ["ZZ", "ZO", "OZ", "OO"]}').as_json)
    end
    it 'should call DataDescriptorModel new when created with   "{\"data\" : [..]}' do
      expect(DataDescriptorModel).to receive(:new).with(EnsureJSON.new('{"data":[]}').as_json)
      DescriptorModelFactory.make_model(EnsureJSON.new('{"data":[]}').as_json)
    end
  end
  context 'input string does not have one of zero, value, classical, qubit or data' do
    example 'input something' do
      expect do
        DescriptorModelFactory.make_model 'somethng'
      end.to raise_error(ModelCreateError)
    end
    example 'input ab<Zero/>' do
      expect do
        DescriptorModelFactory.make_model 'ab<Zero/>'
      end.to raise_error(ModelCreateError)
    end
    example 'input <Alg<Qubit>' do
      expect do
        DescriptorModelFactory.make_model '<Alg<Qubit>'
      end.to raise_error(ModelCreateError)
    end
  end
  it 'embeds the incorrect value when raising a create exception' do
    expect do
      DescriptorModelFactory.make_model 'somethng'
    end.to raise_error(ModelCreateError, /somethng/)
  end
  context 'multiple choices' do
    it 'will still create a model if the string has two choices' do
      res = nil
      expect do
        res = DescriptorModelFactory.make_model '{"data":[],"zero":0}'
      end.not_to raise_error
      expect(res).not_to be_nil
    end
  end
end
