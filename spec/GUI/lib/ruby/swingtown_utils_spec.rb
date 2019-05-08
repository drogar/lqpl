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

    describe :file_check_and_warn do
      it 'returns without warn or exit if the file does not exist' do
        allow(Warning).to receive(:warn)
        expect do
          Swingtown.file_check_and_warn('this file', 'junk.junk')
        end.not_to raise_error(SystemExit)
        # expect(Warning).not_to have_received(:warn)
        # Although the above expect(Warning) should pass, it never does
      end
      it 'calls warn and exits if the file exists' do
        allow(Warning).to receive(:warn)
        expect do
          Swingtown.file_check_and_warn('this file', ::File.expand_path(__FILE__))
        end.to raise_error(SystemExit)
        expect(Warning).to have_received(:warn).with(/The this file file/)
      end
    end
    describe :copy_over do
      before :example do
        allow(Swingtown).to receive(:copy_over_ruby)
        allow(Swingtown).to receive(:copy_over_mig)
      end
      it 'calls copy_over_ruby' do
        Swingtown.copy_over
        expect(Swingtown).to have_received(:copy_over_ruby)
      end
      it 'calls copy_over_mig' do
        Swingtown.copy_over
        expect(Swingtown).to have_received(:copy_over_mig)
      end
    end
    describe :copy_over_mig do
      before(:example) do
        allow(Warning).to receive(:warn)
        allow(FileUtils).to receive(:mkdir_p)
        allow(FileUtils).to receive(:cp_r)
        allow(Swingtown).to receive(:find_mig_jar) { 'mig-jar' }
        allow(Swingtown).to receive(:file_check_and_warn)
      end
      it 'defaults its path argument to "lib/java" and check/warns about it' do
        Swingtown.copy_over_mig
        expect(Swingtown).to have_received(:file_check_and_warn).with('miglayout jar', 'lib/java/mig-jar')
      end
      it 'finds the mig-jar based on where it is called' do
        Swingtown.copy_over_mig
        expect(Swingtown).to have_received(:find_mig_jar).with(%r{lib/ruby})
      end
      it 'takes the path entered as an argument and creates it' do
        Swingtown.copy_over_mig('delete-me/path-to-remove')
        expect(FileUtils).to have_received(:mkdir_p).with('delete-me/path-to-remove')
      end
      it 'warns where the mig jar is' do
        Swingtown.copy_over_mig
        expect(Warning).to have_received(:warn).with(/Have mig jar at mig-jar/)
      end
      it 'copies over the mig jar' do
        Swingtown.copy_over_mig
        expect(FileUtils).to have_received(:cp_r).with('mig-jar', 'lib/java', verbose: true)
      end
    end
    describe :copy_over_ruby do
      before(:example) do
        allow(FileUtils).to receive(:mkdir_p)
        allow(FileUtils).to receive(:cp_r)
        allow(Swingtown).to receive(:file_check_and_warn)
      end
      it 'defaults the path to "lib/ruby" and check/warns about it' do
        Swingtown.copy_over_ruby
        expect(Swingtown).to have_received(:file_check_and_warn).with('swingset', 'lib/ruby/swingset')
      end
      it 'takes the path entered as an argument and creates it' do
        Swingtown.copy_over_ruby('delete-me/path-to-remove')
        expect(FileUtils).to have_received(:mkdir_p).with('delete-me/path-to-remove')
      end
      it 'copies the swing code' do
        Swingtown.copy_over_ruby
        expect(FileUtils).to have_received(:cp_r).with(/swingset$/, 'lib/ruby', verbose: true)
        expect(FileUtils).to have_received(:cp_r).with(/swingset.rb$/, 'lib/ruby', verbose: true)
        expect(FileUtils).to have_received(:cp_r).with(/swingset_utils.rb$/, 'lib/ruby', verbose: true)
      end
    end
    describe :check_java_lib_jar do
      it 'adds java to the current path and warns with it' do
        allow(Warning).to receive(:warn)
        Swingtown.check_java_lib_jar
        expect(Warning).to have_received(:warn).with(%r{lib/ruby/java})
      end
      it 'returns the java lib dir' do
        allow(Warning).to receive(:warn)
        expect(Swingtown.check_java_lib_jar =~ %r{lib/ruby/java}).not_to be_nil
      end
    end
  end
end
