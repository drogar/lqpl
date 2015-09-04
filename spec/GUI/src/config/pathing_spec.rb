# encoding: utf-8
require 'pathing'

describe Pathing do
  it 'is valid' do
    expect(Pathing).not_to be_nil
  end
  subject { Pathing }
  describe :siblings_path do
    it 'adds "**/*" to the end of path' do
      expect(subject.siblings_path(__FILE__)).to match(%r{\*\*/\*$})
    end
    it 'removes the last element of the path' do
      expect(subject.siblings_path(__FILE__)).not_to match(File.dirname(__FILE__))
    end
    path = File.expand_path(File.dirname(__FILE__) + '/..').split('/')
    path.each do |p|
      it "contains the rest of the path: '#{p}'" do
        expect(subject.siblings_path(__FILE__)).to match(p)
      end
    end
  end

  describe :unpercent_spaces do
    it 'translates out %20' do
      expect(subject.unpercent_spaces('%20')).to eq ' '
    end
    it 'handles multiple %20s' do
      expect(subject.unpercent_spaces('%20%20%20')).to eq '   '
    end
    it 'Leaves other chars alone' do
      expect(subject.unpercent_spaces('What%20Ever')).to eq('What Ever')
    end
    it 'works even when there are no percents' do
      expect(subject.unpercent_spaces('stuff with')).to eq('stuff with')
    end
  end
end
