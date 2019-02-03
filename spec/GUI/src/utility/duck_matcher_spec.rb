require 'duck_matcher'

describe DuckMatcher do
  it 'is created from a start and stop string,  followed by a third string' do
    dm = DuckMatcher.new 'start', 'stop', 'entire'
    expect(dm).not_to be_nil
  end
  describe 'match string' do
    before :each do
      @dm = DuckMatcher.new 'strt', 'stop', 'entire'
    end
    it 'should match the start string strt' do
      expect(@dm.matchss.match('strt')).not_to be_nil
    end
    it 'should match the stop string stop' do
      expect(@dm.matchss.match('stop')).not_to be_nil
    end
    it 'should match the entire string entire' do
      expect(@dm.matchss.match('entire')).not_to be_nil
    end
  end
  describe '_matched_paired_start_stop' do
    before :each do
      @dm = DuckMatcher.new 'strt', 'stop', 'entire'
      @dm.recurss = 0
    end
    it 'should increment when passed an enumerable a where a[:strt] is not false' do
      @dm._matched_paired_start_stop?(strt: true)
      expect(@dm.recurss).to eq(1)
    end
    it 'should decrement when passed an enumerable a where a[:stop] is not false' do
      @dm._matched_paired_start_stop?(stop: true)
      expect(@dm.recurss).to eq(-1)
    end
    it 'should do nothing and return true when passed an enumerable
        a where a[:strt] and a[:stop] are false' do
      expect(@dm._matched_paired_start_stop?({})).to be true
    end
  end
  describe 'match' do
    before :each do
      @dm = DuckMatcher.new 'strt', 'stop', 'entire'
    end
    it 'matches for the third element first and returns that if it exists' do
      m = @dm.match 'entire'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('entire')
    end
    it 'returns nil if the input is empty' do
      m = @dm.match ''
      expect(m).to be_nil
    end
    it 'returns nil if the input is not matched' do
      m = @dm.match 'strtstrtstrtstop'
      expect(m).to be_nil
    end
    it 'partitions the strtxxxstop into a 1 element array [strtxxxstop]' do
      m = @dm.match 'strtxxxstop'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('strtxxxstop')
    end
    it 'partitions the strtxxxstopstrtxxxstop into a 1 element array [strtxxxstop]' do
      m = @dm.match 'strtxxxstopstrtxxxstop'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('strtxxxstop')
    end
    it 'partitions the strtxxxstopstrtxxxstopstrtxxxstop into a 1 element array [strtxxxstop]' do
      m = @dm.match 'strtxxxstopstrtxxxstopstrtxxxstop'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('strtxxxstop')
    end
    it 'partitions the strtxxstrtxxxstopxstop into a 1 element array [strtxxstrtxxxstopxstop]' do
      m = @dm.match 'strtxxstrtxxxstopxstop'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('strtxxstrtxxxstopxstop')
    end
    it 'partitions the strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstopstrtxxxstop
        into a 1 element array [strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstop]' do
      m = @dm.match 'strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstopstrtxxxstop'
      expect(m.length).to eq(1)
      expect(m[0]).to eq('strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstop')
    end
  end
end
