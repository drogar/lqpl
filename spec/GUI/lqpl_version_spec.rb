require 'lqpl_version'

describe "LqplVersion" do
  describe 'constants' do
    describe 'LQPL_GUI_VERSION' do
      it 'exists and is set to 4 dot separated digits' do
        expect(LqplVersion::LQPL_GUI_VERSION).to eq('0.9.2.1')
      end
      it 'is frozen' do
        expect(LqplVersion::LQPL_GUI_VERSION).to be_frozen
      end
    end
    describe 'ABOUT_STRING' do
      it 'exists' do
        expect(LqplVersion::ABOUT_STRING).not_to be nil
      end
      it 'is frozen' do
        expect(LqplVersion::ABOUT_STRING).to be_frozen
      end
      it 'is html' do
        expect(LqplVersion::ABOUT_STRING =~ %r{<html>.*</html>}m).not_to be_nil
      end
      it 'contains the version' do
        expect(LqplVersion::ABOUT_STRING =~ /0.9.2.1/).not_to be_nil
      end
      it 'contains the contributors names' do
        expect(LqplVersion::ABOUT_STRING =~ /Brett G\. Giles/).not_to be_nil
        expect(LqplVersion::ABOUT_STRING =~ /Robin Cockett/).not_to be_nil
      end
    end
    describe 'LICENSE_STRING' do
      it 'is html' do
        expect(LqplVersion::LICENSE_STRING =~ %r{<html>.*</html>}m).not_to be_nil
      end
      it 'includes the monkeybars.jar licence note' do
        expect(LqplVersion::LICENSE_STRING =~ /The Ruby license - monkeybars.jar/).not_to be_nil
      end
      it 'includes the jruby-complete jar licence note' do
        expect(LqplVersion::LICENSE_STRING =~ /The Common Public License version 1.0 - jruby-complete.jar/).not_to be_nil
      end
      it 'is frozen' do
        expect(LqplVersion::LICENSE_STRING).to be_frozen
      end
    end
  end
end
