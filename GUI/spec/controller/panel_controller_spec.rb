require 'spec/spec_helper'

describe PanelController do
  describe "class methods" do
    describe "controller_from_name" do
      it "should return ClassicalStack controller for ['anything','Classical','Stack']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "Classical", "Stack"])).to eq(ClassicalStackController)
        end
      end
      it "should return Dump controller for ['anything','Dump']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "Dump"])).to eq(DumpController)
        end
      end
      it "should return Executable Code controller for ['anything','Executing','Code']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "Executing", "Code"])).to eq(ExecutableCodeController)
        end
      end
      it "should return QuantumStack controller for ['anything','Quantum','Stack']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "Quantum", "Stack"])).to eq(QuantumStackController)
        end
      end
      it "should return stacktranslation controller for ['anything','Stack','Translation']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "Stack", "Translation"])).to eq(StackTranslationController)
        end
      end
      it "should return nil for ['anything','something']" do
        SwingRunner::on_edt do
          expect(PanelController::controller_from_name(["junk", "somethin"])).to be_nil
        end
      end
    end
  end
  describe "instance methods" do
    before :each do
      SwingRunner::on_edt do
        @pm = PanelController.instance
      end
    end
    it "should return false for update_on_lqpl_model_trim" do
      SwingRunner::on_edt do
        expect(@pm.update_on_lqpl_model_trim).to be_false
      end
    end
  end
end