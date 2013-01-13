require 'spec/spec_helper'

describe Connection do
  before :each do
    @c = Connection.instance
  end
  after :each do
    @c.close_down
  end
  describe "_make_connection" do
    it "should connect to the default web server when the port is set to 80" do
      @c.port=80
      @c._make_connection.should be_true
    end
    it "should be false when trying to connect to  port 20 (normally unused)" do
      @c.port = 20
      @c._make_connection.should be_false
    end
  end
  describe "_start_up_the_executable_in_a_process" do
    it "should raise an error if the executable is not found" do
      @c.port = 768
      expect {
        @c.connect
      }.to raise_error ServerProcessNotFound, /no process/
    end
  end
  describe "connect" do
    it "should successfully connect to the compiler server on 7683" do
      @c.port = 7683
      @c.connect_to = "lqpl-compiler-server"
      @c.connect
      @c.connected?.should be_true
    end
    it "should raise an error if the port is incorrect" do
      @c.port = 76
      @c.connect_to = "lqpl-compiler-server"
      expect {
        @c.connect
      }.to raise_error ServerProcessNotFound, /76/
    end
    it "should raise an error if the connect to can not be found" do
      @c.port = 768
      @c.connect_to = "junkjunkjunk"
      expect {
        @c.connect
      }.to raise_error ServerProcessNotFound, /junk/
    end
  end
    
end