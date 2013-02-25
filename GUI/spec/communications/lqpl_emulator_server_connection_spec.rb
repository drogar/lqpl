require 'spec/spec_helper'

describe LqplEmulatorServerConnection do
  context "creation" do
    context "enforce singleton" do
      before(:each) do
        @sc = nil
      end
      after(:each) do
        @sc.close_down if @sc
      end
      it "gives an error when trying to create with 'new'" do
        expect {@sc=LqplEmulatorServerConnection.new}.to raise_error NoMethodError
      end
      it "allows default creation with a port of 9502" do
        @sc = LqplEmulatorServerConnection.instance
        @sc.port.should == 9502
      end
    end
    context "connection" do
      before :each do
        @sc = LqplEmulatorServerConnection.get_instance
      end
      after(:all) do
        @sc.close_down if @sc
      end
      it "connects to the lqpl-serv process when created" do
        @sc.connect
        @sc.should be_connected
      end
    end
  end
  context "interfaces with the lqpl-serv" do
    before :each do
        @sc = LqplEmulatorServerConnection.get_instance
        @sc.connect
      end
    
    after :all do
      @sc.close_down if @sc
    end
    it "sends QPO code to the lqpl-serv and gets 'Assembled' back" do
      fname = "#{TEST_QP_PATH}/min.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
      flag.should =~ /Assembled/
    end
  end
  context "execution control" do
    before(:each) do
      @sc = LqplEmulatorServerConnection.get_instance
      @sc.connect
      fname = "#{TEST_QP_PATH}/coin.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
    end
    after :all do
      @sc.close_down if @sc
    end
    specify {@sc.send_set_depth_multiplier.should =~ /reset/}
    specify {@sc.do_step.should =~ /Stepped/}
    specify {@sc.do_step(5).should =~ /Stepped/}
    specify {sc.do_run.should =~ /executed/}
    specify {@sc.do_trim.should =~ /trimmed/}
    it "steps through a program and gives a different status when at the end" do
      @sc.do_step(40).should =~ /executed/}
    end
    it "should allow a step or another run after a run and stil return executed" do
      @sc.do_run
      @sc.do_step.should =~ /executed/
      @sc.do_run.should =~ /executed/
    end
  end
  context "gets the data" do
    before(:each) do
      @sc = LqplEmulatorServerConnection.instance
      @sc.connect
      fname = "#{TEST_QP_PATH}/coin.reference.qpo"
      flag = @sc.send_load_from_file(10,fname)
      @sc.do_step(10) # down one branch of the measure
    end
    after :all do
      @sc.close_down if @sc
    end
    specify {@sc.get_qstack.should =~ /<Qstack/}
    specify {@sc.get_classical_stack.should =~ /<Classical/}
    specify {@sc.get_dump.should =~ /<Dump/}
    specify {@sc.code_pointer.should =~ /<pair/}
    specify {@sc.loaded_code.should =~ /<Code/}
    specify {@sc.get_stack_translation.should =~ /<MMap/}
    specify {@sc.get_simulate_results.should =~ /<Simulated><double/}
  end
end