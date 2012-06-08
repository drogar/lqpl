require 'spec/spec_helper'

describe ServerConnection do
  context "creation" do
    context "enforce singleton" do
      before(:each) do
        @sc = nil
      end
      after(:each) do
        @sc.close_down if @sc
      end
      it "gives an error when trying to create with 'new'" do
        expect {@sc=ServerConnection.new}.to raise_error NoMethodError
      end
      it "allows default creation with a port of 9502" do
        @sc = ServerConnection.instance
        @sc.port.should == 9502
      end
    end
    context "connection" do
      before :each do
        @sc = ServerConnection.instance
      end
      after(:each) do
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
        @sc = ServerConnection.instance
        @sc.connect
      end
    it "sends QPO code to the lqpl-serv and gets 'Assembled' back" do
      fname = "#{Dir.pwd}/testdata/qplprograms/min.reference.qpo"
      flag = @sc.send_load_from_file fname
      flag.should =~ /Assembled/
    end
  end
  context "execution control" do
    before(:each) do
      @sc = ServerConnection.instance
      @sc.connect
      fname = "#{Dir.pwd}/testdata/qplprograms/coin.reference.qpo"
      flag = @sc.send_load_from_file fname
    end

    it "steps through a program" do
      @sc.do_step.should =~ /Stepped/
      @sc.do_step(5).should =~ /Stepped/
    end
    it "steps through a program and gives a different status when at the end" do
      @sc.do_step(40).should =~ /executed/
    end
    it "runs a program and gets the status 'executed'" do
      @sc.do_run.should =~ /executed/
    end
    it "should allow a step or another run after a run and stil return executed" do
      @sc.do_run
      @sc.do_step.should =~ /executed/
      @sc.do_run.should =~ /executed/
    end
  end
  context "gets the data" do
    before(:each) do
      @sc = ServerConnection.instance
      @sc.connect
      fname = "#{Dir.pwd}/testdata/qplprograms/coin.reference.qpo"
      flag = @sc.send_load_from_file fname
      @sc.do_step(10) # down one branch of the measure
    end
    it "returns the qstack" do
      @sc.get_qstack[0].should =~ /<Qstack/
    end
    it "returns the classical_stack" do
      @sc.get_classical_stack.should =~ /<Cstack/
    end
    it "returns the dump" do
      @sc.get_dump.should =~ /<Dump/
    end
    it "returns the code pointer" do
      @sc.code_pointer.should =~ /<pair/
    end
    it "returns the code" do
      @sc.loaded_code.should =~ /<Code/
    end
    it "returns the stack translation" do
      @sc.get_stack_translation.should =~ /<MMap/
    end

  end
end