require 'swingtown_utils'
describe Swingtown do
  describe 'constants' do
    it 'sets the VERSION to 0.3.0' do
      expect(Swingtown::VERSION).to eq '0.3.0'
    end
    it 'sets PATH' do
      expect(Swingtown::PATH =~ %r{GUI/lib/$}).not_to be_nil
    end
    it 'sets LIBPATH' do
      expect(Swingtown::LIBPATH).to eq(Swingtown::PATH + 'ruby/')
    end
  end
  describe 'module methods' do
    it 'has a version method that returns the VERSION' do
      expect(Swingtown.version).to eq Swingtown::VERSION
    end
    describe :libpath do
      it 'returns LIBPATH with no args' do
        expect(Swingtown.libpath).to eq Swingtown::LIBPATH
      end
      it 'joins any args with file separator and adds to the FILEPATH' do
        fs = ::File::SEPARATOR
        expect(Swingtown.libpath('a')).to eq Swingtown::LIBPATH + 'a'
        expect(Swingtown.libpath('a', 'b')).to eq Swingtown::LIBPATH + 'a' + fs + 'b'
        expect(Swingtown.libpath('a', 'b', 'cdefg')).to eq Swingtown::LIBPATH + 'a' + fs + 'b' + fs + 'cdefg'
      end
      describe :path do
        it 'returns PATH with no args' do
          expect(Swingtown.path).to eq Swingtown::PATH
        end
        it 'joins any args with file separator and adds to the FILEPATH' do
          fs = ::File::SEPARATOR
          expect(Swingtown.path('a')).to eq Swingtown::PATH + 'a'
          expect(Swingtown.path('a', 'b')).to eq Swingtown::PATH + 'a' + fs + 'b'
          expect(Swingtown.path('a', 'b', 'cdefg')).to eq Swingtown::PATH + 'a' + fs + 'b' + fs + 'cdefg'
        end
      end
    end
    describe :find_mig_jar do
      # under GUI/devlib
      let(:currloc) { Swingtown::LIBPATH }
      it 'raises an error if not found' do
        expect do
          Swingtown.find_mig_jar(currloc)
        end.to raise_error(/Failed to find MiG layout jar/)
      end
      it 'returns the loc if it is found' do
        devlib = ::File.join(::File.dirname(Swingtown::PATH), 'devlib')
        expect(Swingtown.find_mig_jar(devlib) =~ %r{devlib/java/miglayout}).not_to be_nil
      end
    end
    # TODO: figure out how to dummy warn and exit so I can test thre remaining functions
    describe :file_check_and_warn do
      warn_value = nil
      exit_called = false
      # def warn(arg)
      #        warn_value = arg
      # end

      # def exit
      #  exit_called = true
      # end
      before do
        warn_value = nil
        exit_called = false
      end
      pending 'calls warn if the file exists' do
        expect(false).to be true
        # Swingtown.file_check_and_warn('fake', ::File.expand_path(__FILE__))
        # expect(warn_value =~ /The fake file/).not_to be_nil
        # expect(exit_called).to be true
      end
    end
  end
end
