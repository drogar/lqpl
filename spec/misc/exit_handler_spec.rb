require 'spec/spec_helper'

describe ExitHandler do
  describe 'close_servers' do
    it 'should close the compiler server connection' do
      ExitHandler.instance.close_servers
      expect(CompilerServerConnection.instance).not_to be_connected
    end

    it 'should close the emulator server connection' do
      ExitHandler.instance.close_servers
      expect(LqplEmulatorServerConnection.instance).not_to be_connected
    end
  end
  describe 'handleQuitRequestWith' do
    before (:each) do
      @qr = double('quit_response')
      expect(@qr).to receive(:performQuit)
    end
    it 'should call performQuit on the quit response' do
      ExitHandler.instance.handle_quit_request_with(nil, @qr)
    end
    it 'should close the compiler server  connection' do
      ExitHandler.instance.handle_quit_request_with(nil, @qr)
      expect(CompilerServerConnection.instance).not_to be_connected
    end

    it 'should close the emulator server connection' do
      ExitHandler.instance.handle_quit_request_with(nil, @qr)
      expect(LqplEmulatorServerConnection.instance).not_to be_connected
    end
  end
end
