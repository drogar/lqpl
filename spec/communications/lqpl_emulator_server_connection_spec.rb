# Encoding UTF-8
require 'spec/spec_helper'

describe LqplEmulatorServerConnection do
  context 'creation' do
    context 'enforce singleton' do
      before :each do
        @sc = nil
      end
      after :each do
        @sc.close_down if @sc
      end
      it 'gives an error when trying to create with new' do
        expect {@sc=LqplEmulatorServerConnection.new}.to raise_error NoMethodError
      end
      it 'allows default creation with a port of 9502' do
        @sc = LqplEmulatorServerConnection.instance
        expect(@sc.port).to eq(9502)
      end
    end
    context 'connection' do
      before :each do
        @sc = LqplEmulatorServerConnection.get_instance
      end
      after :all do
        @sc.close_down if @sc
      end
      it 'connects to the lqpl-serv process when created' do
        @sc.connect
        expect(@sc).to be_connected
      end
    end
  end
  context 'interfaces with the lqpl-serv' do
    before :each do
        @sc = LqplEmulatorServerConnection.get_instance
        @sc.connect
      end

    after :all do
      @sc.close_down if @sc
    end
    it 'sends QPO code to the lqpl-serv and gets Assembled back' do
      fname = "#{TEST_QP_PATH}/min.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
      expect(flag).to match(/Assembled/)
    end
  end
  context 'execution control' do
    before :each  do
      @sc = LqplEmulatorServerConnection.get_instance
      @sc.connect
      fname = "#{TEST_QP_PATH}/coin.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
    end
    after :all do
      @sc.close_down if @sc
    end
    specify {expect(@sc.send_set_depth_multiplier).to match(/reset/)}
    specify {expect(@sc.do_step).to match(/Stepped/)}
    specify {expect(@sc.do_step(5)).to match(/Stepped/)}
    specify {expect(@sc.do_run).to match(/executed/)}
    specify {expect(@sc.do_trim).to match(/trimmed/)}
    it 'steps through a program and gives a different status when at the end' do
      expect(@sc.do_step(40)).to match(/executed/)
    end
    it 'should allow a step or another run after a run and stil return executed' do
      @sc.do_run
      expect(@sc.do_step).to match(/executed/)
      expect(@sc.do_run).to match(/executed/)
    end
  end
  context 'gets the data' do
    before :each do
      @sc = LqplEmulatorServerConnection.instance
      @sc.connect
      fname = "#{TEST_QP_PATH}/coin.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
      @sc.do_step(10) # down one branch of the measure
    end
    after :all do
      @sc.close_down if @sc
    end
    specify {expect(@sc.get_qstack).to match(/<Qstack/)}
    specify {expect(@sc.get_classical_stack).to match(/<Classical/)}
    specify {expect(@sc.get_dump).to match(/<Dump/)}
    specify {expect(@sc.code_pointer).to match(/<pair/)}
    specify {expect(@sc.loaded_code).to match(/<Code/)}
    specify {expect(@sc.get_stack_translation).to match(/<MMap/)}
    specify {expect(@sc.get_simulate_results).to match(/<Simulated><double/)}
  end
end
