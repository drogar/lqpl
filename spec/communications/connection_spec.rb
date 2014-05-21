# encoding: utf-8
require 'spec/spec_helper'

describe Connection do
  before :each do
    @c = Connection.instance
    @c.connect_to = 'What'
  end
  after :each do
    @c.close_down
  end
  describe '_make_connection' do
    it 'should return [] when it actually connects' do
      @c.port = 80
      expect(TCPSocket).to receive(:new).with(Connection::LOCAL_CONNECTS[0], 80).and_return(nil)
      expect(@c._make_connection).to eql([])
    end
    it 'should return refused if trying to connect to an unused port' do
      @c.port = 20
      expect(TCPSocket).to receive(:new).with(Connection::LOCAL_CONNECTS[0], 20)
        .and_raise(Errno::ECONNREFUSED)
      res = @c._make_connection
      expect(res[0]).to match('Connect refused For')
    end
    it 'should return error if getting a socket error' do
      @c.port = 20
      expect(TCPSocket).to receive(:new).with(Connection::LOCAL_CONNECTS[0], 20)
        .and_raise(SocketError)
      res = @c._make_connection
      expect(res[0]).to match('Socket error for')
    end
  end
  describe 'connect' do
    it 'should raise an error if the executable is not found' do
      @c.port = 768
      expect { @c.connect }.to raise_error ServerProcessNotFound, /no process/
    end
    it 'should successfully connect to the compiler server on 7683' do
      @c.port = 7683
      @c.connect_to = 'lqpl-compiler-server'
      @c.connect
      expect(@c.connected?).to be true
    end
    it 'should raise an error if the port is incorrect' do
      @c.port = 76
      @c.connect_to = 'lqpl-compiler-server'
      expect { @c.connect }.to raise_error ServerProcessNotFound, /76/
    end
    it 'should raise an error if the connect to can not be found' do
      @c.port = 768
      @c.connect_to = 'junkjunkjunk'
      expect { @c.connect }.to raise_error ServerProcessNotFound, /junk/
    end
  end
  context 'send and receive' do
    subject { Connection.instance }
    let(:conn) { double('c') }
    before(:each) do
      allow(subject).to receive(:connection).and_return(conn)
      allow(conn).to receive(:close)
    end
    describe :send_data do
      it 'should send a command' do
        expect(conn).to receive(:puts).with('arg')
        subject.send_data('arg')
      end
    end

    describe :read_data do
      it 'should get data' do
        expect(conn).to receive(:readline).and_return('read a line')
        expect(subject.read_data).to eq('read a line')
      end
    end
    describe 'send_and_read_data' do
      it 'should send a command and get output' do
        expect(conn).to receive(:puts).with('arg')
        expect(conn).to receive(:readline).and_return('read a line')
        expect(subject.send_and_read_data('arg')).to eq('read a line')
      end
    end
  end
end
