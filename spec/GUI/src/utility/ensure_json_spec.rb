describe EnsureJSON do
  context 'creation' do
    it 'accepts a string for new' do
      expect(EnsureJSON.new('{"json":0}')).to be_a(EnsureJSON)
    end
    it 'accepts a hash in new' do
      expect(EnsureJSON.new(json: 0)).to be_a(EnsureJSON)
    end
    it 'throws an error for nonstring or nonhash' do
      expect do
        EnsureJSON.new(0)
      end.to raise_error ModelCreateError, /0.*String or Hash/
    end
  end
  context 'created with string' do
    it 'throws a json parseerror for non json syntax' do
      expect do
        EnsureJSON.new('just garbage')
      end.to raise_error JSON::ParserError, /just garbage/
    end
    describe :as_json do
      it 'returns a hash of the json' do
        expect(EnsureJSON.new('{"json":0}').as_json).to eql(json: 0)
      end
    end
  end
  context 'created with a hash' do
    describe :as_json do
      it 'returns the hash passed in on create' do
        expect(EnsureJSON.new(json: 0).as_json).to eql(json: 0)
      end
    end
  end
end
