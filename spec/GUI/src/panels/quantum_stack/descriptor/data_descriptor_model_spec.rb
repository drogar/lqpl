require 'quantum_stack_model'

DATA_NIL = '{"data":[{"cons":"Nil", "addresses" : []}]}'.freeze
DATA_TWO = '{"data":[{"cons":"Nil", "addresses" : []},{"cons":"Nil2", "addresses" : []}]}'.freeze
DATA_NILC = '{"data":[{"cons":"Nil", "addresses" : []},{"cons":"C", "addresses" : [3,4]}]}'.freeze
describe DataDescriptorModel do
  describe 'invalid input' do
    it 'should raise an error if constructed with incorrect top level' do
      expect do
        DescriptorModelFactory.make_model '{"datwa":[{"cons":"Nil", "addressess" : []}]}'
      end.to raise_error(ModelCreateError, /datwa/)
    end
    it 'should raise an error if constructed with incorrect data' do
      expect do
        DescriptorModelFactory.make_model '{"data":[{"cons":"Nil","addresses":["34", "ab"]}]}'
      end.to raise_error(ModelCreateError, /ab/)
    end
    it 'should raise an error if incorrect keys in the pair' do
      expect do
        DescriptorModelFactory.make_model '{"data":[{"cons":"Nil", "adqw3ses" : []}]}'
      end.to raise_error(ModelCreateError, /adqw3ses/)
    end
  end
  it 'should have a length equal to the number of elements of the passed in list' do
    sd = DescriptorModelFactory.make_model DATA_NIL
    expect(sd.length).to eql(1)
    sd = DescriptorModelFactory.make_model DATA_TWO
    expect(sd.length).to eql(2)
  end
  it 'should have the value being the map of constructor/address pairs in the string' do
    sd = DescriptorModelFactory.make_model DATA_NILC
    expect(sd.value).to eq([{ cons: 'Nil', addresses: [] },
                            { cons: 'C', addresses: [3, 4] }])
  end
  it 'should return a list of length length when asked for substack labels' do
    sd = DescriptorModelFactory.make_model DATA_NIL
    expect(sd.substack_labels.length).to eql(1)
    sd = DescriptorModelFactory.make_model DATA_NILC
    expect(sd.substack_labels.length).to eql(2)
  end

  it 'should have constructer names only when there are no stack addresses' do
    sd = DescriptorModelFactory.make_model DATA_NIL
    expect(sd.substack_labels).to eq(['Nil'])
    sd = DescriptorModelFactory.make_model DATA_TWO
    expect(sd.substack_labels).to eq(%w(Nil Nil2))
  end

  it 'should have the substack_labels with constructors with addresses in brackets' do
    sd = DescriptorModelFactory.make_model DATA_NILC
    expect(sd.substack_labels).to eq(['Nil', 'C[3, 4]'])
  end

  context 'class methods' do
    context 'validation' do
      it 'should not raise an error if passed an array with elements' do
        expect(DataDescriptorModel.validate_substacks_count([1, 2])).to be_nil
      end
      it 'should raise an error if passed an empty array' do
        expect do
          DataDescriptorModel.validate_substacks_count([])
        end.to raise_error ModelCreateError, /Data.*should have/
      end
      it 'should raise an error if passed a nil array' do
        expect do
          DataDescriptorModel.validate_substacks_count(nil)
        end.to raise_error ModelCreateError, /Data.*should have/
      end
    end
  end
end
