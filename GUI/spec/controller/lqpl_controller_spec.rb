require 'spec/spec_helper'



class Drunner < GuiQuery
  # Launch the app in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  # FEST will call this method for us before the test.
  #
  def initialize(lqplinst)
    super()
    @l = lqplinst
  end
  def executeInEDT
    @l.load()
    @l.file_compile_action_performed
  end
end

describe LqplController do
  before :each do
    SwingRunner::on_edt do
      @l = LqplController.instance
    end
  end
  describe "load" do
    before (:each) do
      SwingRunner::on_edt do
        @l.load()
      end
    end
    it "sets up the cmp and ensures it is connected" do
      @l.cmp.should_not be_nil
      @l.cmp.should be_connected
    end
    it "ensures the server connection is connected" do
      @l.lqpl_emulator_server_connection.should be_connected
    end
    it "sets up the subcontrollers" do
      @l.sub_controllers.size.should == 5
    end
    it "sets each sub controller to a non-nil value" do
        @l.sub_controllers.each { |c|   c.should_not be_nil }
    end
    it "sets up the dialogs" do
      @l.dialogs.size.should == 2
    end
    it "sets each dialog to a non-nil value" do
      @l.dialogs.each { |d|  d.should_not be_nil }
    end
  end
  describe "file_exit" do
    it "closes the server connection" do
      SwingRunner::on_edt do
        @l.load
        @l.file_exit_action_performed()
      end
      @l.cmp.should_not be_connected
      @l.lqpl_emulator_server_connection(false).should_not be_connected
    end
  end
  describe "all_controllers_dispose" do
    before :each do
      SwingRunner::on_edt do
        @l.load()
      end 
      d1=double("dialog1")
      d1.should_receive(:dispose)
      d2=double("dialog2")
      d2.should_receive(:dispose)
      s1=double("sub1")
      s1.should_receive(:dispose)
      s2=double("sub2")
      s2.should_receive(:dispose)
      @l.dialogs = [d1,d2]
      @l.sub_controllers = [s1,s2]
    end
    after(:each) do
      @l.dialogs = []
      @l.sub_controllers = []
    end
    it "should send 'dispose' to each member of the dialogs and subcontrollers" do
      @l.all_controllers_dispose
    end
  end
  describe "close" do
    before :each do
      SwingRunner::on_edt do
        @l.load()
      end 
      d1=double("dialog1")
      d1.should_receive(:dispose)
      d2=double("dialog2")
      d2.should_receive(:dispose)
      s1=double("sub1")
      s1.should_receive(:dispose)
      s2=double("sub2")
      s2.should_receive(:dispose)
      @l.dialogs = [d1,d2]
      @l.sub_controllers = [s1,s2]
    end
    after(:each) do
      @l.dialogs = []
      @l.sub_controllers = []
    end
    it "should send 'dispose' to each member of the dialogs and subcontrollers" do
      @l.close
    end
    it "closes the server connection" do
      @l.close()
      @l.cmp.should_not be_connected
      @l.lqpl_emulator_server_connection(false).should_not be_connected
    end
  end
  
  describe "sub_controllers_open" do
    before :each do
      s1=double("sub1")
      s1.should_receive(:open)
      s2=double("sub2")
      s2.should_receive(:open)
      @l.sub_controllers = [s1,s2]
    end
    after(:each) do
      @l.dialogs = []
      @l.sub_controllers = []
    end
    it "should send 'open' to each member of the subcontrollers" do
      @l.open_sub_panels
    end
  end
  
  describe "update_sub_model_data" do
    before :each do
      s1=double("sub1")
      s1.should_receive(:set_data_from_lqpl_model)
      s2=double("sub2")
      s2.should_receive(:set_data_from_lqpl_model)
      @l.sub_controllers = [s1,s2]
    end
    after(:each) do
      @l.dialogs = []
      @l.sub_controllers = []
    end
    it "should send 'set_data_from_lqpl_model' to each member of the subcontrollers" do
      @l.update_sub_model_data
    end
  end
  
  describe "trim_button_action_performed" do
    before :each do
      s1=double("sub1")
      s1.should_receive(:set_data_from_lqpl_model)
      s1.should_receive(:update_on_lqpl_model_trim).and_return(true)
      s2=double("sub2")
      s2.should_receive(:update_on_lqpl_model_trim).and_return(false)
      s2.should_not_receive(:set_data_from_lqpl_model)
      @l.sub_controllers = [s1,s2]
    end
    after(:each) do
      @l.dialogs = []
      @l.sub_controllers = []
    end
    it "should send 'set_data_from_lqpl_model' to sub1 and not to sub2" do
      SwingRunner::on_edt do
        @l.trim_button_action_performed
      end
    end
  end 
  
end