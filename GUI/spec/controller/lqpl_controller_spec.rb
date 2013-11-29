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
  describe "after load" do
    before (:each) do
      SwingRunner::on_edt do
        @l.load()
      end
    end
    
    specify {expect(@l.size).to eq(5)}
    specify {expect(@l.size).to eq(2)}
    context "sub_controllers" do
      specify {expect(@l.sub_controllers.compact.size).to eq(5)}
    end
    context "dialogs" do
      specify {expect(@l.dialogs.compact.size).to eq(2)}
    end
    
    context "the compiler server" do
      specify {expect(@l.cmp).not_to be_nil}
      specify {expect(@l.cmp).to be_connected}
    end
    context "the emulator server" do
      specify {expect(@l.lqpl_emulator_server_connection).to be_connected}
    end
  end
  describe "file_exit" do
    it "closes the server connection" do
      SwingRunner::on_edt do
        @l.load
        @l.file_exit_action_performed()
      end
      expect(@l.cmp).not_to be_connected
      expect(@l.lqpl_emulator_server_connection(false)).not_to be_connected
    end
  end
  describe "all_controllers_dispose" do
    before :each do
      SwingRunner::on_edt do
        @l.load()
      end 
      d1=double("dialog1")
      expect(d1).to receive(:dispose)
      d2=double("dialog2")
      expect(d2).to receive(:dispose)
      s1=double("sub1")
      expect(s1).to receive(:dispose)
      s2=double("sub2")
      expect(s2).to receive(:dispose)
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
      expect(d1).to receive(:dispose)
      d2=double("dialog2")
      expect(d2).to receive(:dispose)
      s1=double("sub1")
      expect(s1).to receive(:dispose)
      s2=double("sub2")
      expect(s2).to receive(:dispose)
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
      expect(@l.cmp).not_to be_connected
      expect(@l.lqpl_emulator_server_connection(false)).not_to be_connected
    end
  end
  
  describe "sub_controllers_open" do
    before :each do
      s1=double("sub1")
      expect(s1).to receive(:open)
      s2=double("sub2")
      expect(s2).to receive(:open)
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
      expect(s1).to receive(:set_data_from_lqpl_model)
      s2=double("sub2")
      expect(s2).to receive(:set_data_from_lqpl_model)
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
      expect(s1).to receive(:set_data_from_lqpl_model)
      expect(s1).to receive(:update_on_lqpl_model_trim).and_return(true)
      s2=double("sub2")
      expect(s2).to receive(:update_on_lqpl_model_trim).and_return(false)
      expect(s2).not_to receive(:set_data_from_lqpl_model)
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