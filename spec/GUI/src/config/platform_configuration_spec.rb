require 'config/platform_configuration'

describe PlatformConfiguration do
  it 'is valid' do
    expect(PlatformConfiguration).not_to be_nil
  end
  describe 'mac based' do
    subject do
      mac = instance_double(ArchitectureCategory, mac?: true, windows?: false, linux?: false)
      tester = instance_double(TestingFest, testing?: false, not_testing?: true)
      PlatformConfiguration.new(mac, tester)
    end

    describe 'on_mac' do
      it 'will yield' do
        expect { |b| subject.on_mac(&b) }.to yield_control
      end
    end
    describe 'on_linux' do
      it ' will not yield' do
        expect { |b| subject.on_linux(&b) }.not_to yield_control
      end
    end
    describe 'on_win' do
      it ' will not yield' do
        expect { |b| subject.on_win(&b) }.not_to yield_control
      end
    end
    describe 'side effects' do
      it 'does set use screen menu bar' do
        java.lang.System.clear_property('apple.laf.useScreenMenuBar')
        subject
        expect(java.lang.System.get_property('apple.laf.useScreenMenuBar').to_s).to eq('true')
      end
      describe 'when testing' do
        subject do
          mac = instance_double(ArchitectureCategory, mac?: true, windows?: false, linux?: false)
          tester = instance_double(TestingFest, testing?: true, not_testing?: false)
          PlatformConfiguration.new(mac, tester)
        end
        it 'does not set to use the screen menu bar' do
          java.lang.System.clear_property('apple.laf.useScreenMenuBar')
          subject
          expect(java.lang.System.get_property('apple.laf.useScreenMenuBar')).to be nil
        end
      end
    end
  end
  describe :not_on_mac do
    describe '(not mac, not testing)' do
      subject do
        nmac = instance_double(ArchitectureCategory, mac?: false, windows?: false, linux?: false)
        tester = instance_double(TestingFest, testing?: false, not_testing?: false)
        PlatformConfiguration.new(nmac, tester)
      end
      it 'does yield ' do
        expect { |b| subject.not_on_mac(&b) }.to yield_control
      end
    end
    describe '(not mac, testing)' do
      subject do
        nmac = instance_double(ArchitectureCategory, mac?: false, windows?: false, linux?: false)
        tester = instance_double(TestingFest, testing?: true, not_testing?: false)
        PlatformConfiguration.new(nmac, tester)
      end
      it 'does yield' do
        expect { |b| subject.not_on_mac(&b) }.to yield_control
      end
    end
    describe '(mac, not testing)' do
      subject do
        nmac = instance_double(ArchitectureCategory, mac?: true, windows?: false, linux?: false)
        tester = instance_double(TestingFest, testing?: false, not_testing?: false)
        PlatformConfiguration.new(nmac, tester)
      end
      it 'does not yield ' do
        expect { |b| subject.not_on_mac(&b) }.not_to yield_control
      end
    end
    describe '(mac, testing)' do
      subject do
        nmac = instance_double(ArchitectureCategory, mac?: true, windows?: false, linux?: false)
        tester = instance_double(TestingFest, testing?: true, not_testing?: false)
        PlatformConfiguration.new(nmac, tester)
      end
      it 'does yield' do
        expect { |b| subject.not_on_mac(&b) }.to yield_control
      end
    end
  end
  describe 'linux based' do
    subject do
      lin = instance_double(ArchitectureCategory, mac?: false, windows?: false, linux?: true)
      PlatformConfiguration.new(lin)
    end
    describe 'on_mac' do
      it 'will not yield' do
        expect { |b| subject.on_mac(&b) }.not_to yield_control
      end
    end
    describe 'on_linux' do
      it ' will  yield' do
        expect { |b| subject.on_linux(&b) }.to yield_control
      end
    end
    describe 'on_win' do
      it ' will not yield' do
        expect { |b| subject.on_win(&b) }.not_to yield_control
      end
    end
  end
  describe 'Windows based' do
    subject do
      win = instance_double(ArchitectureCategory, mac?: false, windows?: true, linux?: false)
      PlatformConfiguration.new(win)
    end
    describe 'on_mac' do
      it 'will not yield' do
        expect { |b| subject.on_mac(&b) }.not_to yield_control
      end
    end
    describe 'on_linux' do
      it 'will not yield' do
        expect { |b| subject.on_linux(&b) }.not_to yield_control
      end
    end
    describe 'on_win' do
      it ' will yield' do
        expect { |b| subject.on_win(&b) }.to yield_control
      end
    end
  end
end
