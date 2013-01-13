require 'spec/spec_helper'

describe PanelController do
  describe "class methods" do
    describe "controller_from_name" do
      it "should return ClassicalStack controller for ['anything','Classical','Stack']" do
        PanelController::controller_from_name(["junk", "Classical", "Stack"]).should == ClassicalStackController
      end
      it "should return Dump controller for ['anything','Dump']" do
        PanelController::controller_from_name(["junk", "Dump"]).should == DumpController
      end
      it "should return Executable Code controller for ['anything','Executing','Code']" do
        PanelController::controller_from_name(["junk", "Executing", "Code"]).should == ExecutableCodeController
      end
      it "should return QuantumStack controller for ['anything','Quantum','Stack']" do
        PanelController::controller_from_name(["junk", "Quantum", "Stack"]).should == QuantumStackController
      end
      it "should return stacktranslation controller for ['anything','Stack','Translation']" do
        PanelController::controller_from_name(["junk", "Stack", "Translation"]).should == StackTranslationController
      end
      it "should return nil for ['anything','something']" do
        PanelController::controller_from_name(["junk", "somethin"]).should be_nil
      end
    end
  end
  describe "instance methods" do
    before :each do
      @pm = PanelController.instance
    end
    it "should return false for update_on_lqpl_model_trim" do
      @pm.update_on_lqpl_model_trim.should be_false
    end
  end
end