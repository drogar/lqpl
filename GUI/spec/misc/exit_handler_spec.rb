require 'spec/spec_helper'

describe ExitHandler do
  describe "close_servers" do
    it "should close the compiler server connection" do
      ExitHandler.instance.close_servers
      CompilerServerConnection.instance.connected?.should be_false
    end
    
    it "should close the emulator server connection" do
      ExitHandler.instance.close_servers
      LqplEmulatorServerConnection.instance.connected?.should be_false
    end
  end
  describe "handleQuitRequestWith" do
    before (:each) do
      @qr=double("quit_response")
      @qr.should_receive(:performQuit)
    end
    it "should call performQuit on the quit response" do
      ExitHandler.instance.handleQuitRequestWith(nil,@qr)
    end
    it "should close the compiler server  connection" do
      ExitHandler.instance.handleQuitRequestWith(nil,@qr)
      CompilerServerConnection.instance.connected?.should be_false
    end
    
    it "should close the emulator server connection" do
      ExitHandler.instance.handleQuitRequestWith(nil,@qr)
      LqplEmulatorServerConnection.instance.connected?.should be_false
    end
  end
end