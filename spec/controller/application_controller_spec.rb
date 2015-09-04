describe ApplicationController do
  describe 'lqpl_emulator_server_connection' do
    before :each do
      @ac = ApplicationController.instance
      LqplEmulatorServerConnection.instance.close_down
    end
    it 'should return a connected instance with no args' do
      l = @ac.lqpl_emulator_server_connection
      expect(l).not_to be_nil
      expect(l).to be_connected
    end
    it 'should return an unconnected instance with connect arg (1st) false' do
      l = @ac.lqpl_emulator_server_connection false
      expect(l).not_to be_nil
      expect(l).not_to be_connected
    end
  end
end
