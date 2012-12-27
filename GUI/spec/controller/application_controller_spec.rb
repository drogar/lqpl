require 'spec/spec_helper'

describe ApplicationController do
  describe "class methods" do
    describe "controller_from_name" do
      it "should return ClassicalStack controller for ['anything','Classical','Stack']" do
        ApplicationController::controller_from_name(["junk", "Classical", "Stack"]).should == ClassicalStackController
      end
      it "should return Dump controller for ['anything','Dump']" do
        ApplicationController::controller_from_name(["junk", "Dump"]).should == DumpController
      end
      it "should return Executable Code controller for ['anything','Executing','Code']" do
        ApplicationController::controller_from_name(["junk", "Executing", "Code"]).should == ExecutableCodeController
      end
      it "should return QuantumStack controller for ['anything','Quantum','Stack']" do
        ApplicationController::controller_from_name(["junk", "Quantum", "Stack"]).should == QuantumStackController
      end
      it "should return stacktranslation controller for ['anything','Stack','Translation']" do
        ApplicationController::controller_from_name(["junk", "Stack", "Translation"]).should == StackTranslationController
      end
      it "should return nil for ['anything','something']" do
        ApplicationController::controller_from_name(["junk", "somethin"]).should be_nil
      end
    end
  end
end