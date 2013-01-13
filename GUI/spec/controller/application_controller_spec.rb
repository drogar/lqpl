require 'spec/spec_helper'

describe ApplicationController do
  describe "lqpl_emulator_server_connection" do
    before :each do
      @ac = ApplicationController.instance
      LqplEmulatorServerConnection.instance.close_down()
    end
    it "should return a connected instance with no args" do
      l = @ac.lqpl_emulator_server_connection
      l.should_not be_nil
      l.should be_connected
    end
    it "should return an unconnected instance with connect arg (1st) false" do
      l = @ac.lqpl_emulator_server_connection false
      l.should_not be_nil
      l.should_not be_connected
    end
  end
      
end