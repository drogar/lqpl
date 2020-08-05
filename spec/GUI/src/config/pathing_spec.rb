require 'config/pathing'

describe Pathing do
  it 'is valid' do
    expect(Pathing).not_to be_nil
  end
  describe 'instance methods' do
    subject { Pathing.new }
    let(:here) { __dir__ }
    describe :pathing_location do
      it 'is the path of the pathing.rb file' do

      end
    end
    describe :base_path do
      it 'is the path of the file' do
        expect(subject.base_path).to(
          eq(File.dirname(File.realpath(here + '/../../../GUI/src')))
        )
      end
    end
  end
end
