require 'spec/spec_helper'

describe PanelController do
  describe 'class methods' do
    describe 'class_name_to_sym' do
      it 'creates a symbol of the name minus controller' do
        expect(PanelController.class_name_to_sym(DumpController)).to eq(:Dump)
      end
      it 'creates a symbol of the name being the first Word in SnakeCase' do
        class SomeThingElse ; end
        expect(PanelController.class_name_to_sym(SomeThingElse)).to eq(:Some)
      end
    end
    describe 'controller_from_name' do
      before(:each) do
        class ClassicalStackController < PanelController ; end
        class DumpController < PanelController ; end
        class ExecutingCodeController < PanelController ; end
        class StackTranslationController < PanelController ; end
        class QuantumStackController < PanelController ; end
      end
      it "should return ClassicalStack controller for ['anything','Classical','Stack']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk Classical Stack))).to eq(ClassicalStackController)
        end
      end
      it "should return Dump controller for ['anything','Dump']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk Dump))).to eq(DumpController)
        end
      end
      it "should return Executable Code controller for ['anything','Executing','Code']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk Executing Code))).to eq(ExecutingCodeController)
        end
      end
      it "should return QuantumStack controller for ['anything','Quantum','Stack']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk Quantum Stack))).to eq(QuantumStackController)
        end
      end
      it "should return stacktranslation controller for ['anything','Stack','Translation']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk Stack Translation))).to eq(StackTranslationController)
        end
      end
      it "should return nil for ['anything','something']" do
        SwingRunner.on_edt do
          expect(PanelController.controller_from_name(%w(junk somethin))).to be_nil
        end
      end
    end
  end
  describe 'instance methods' do
    before :each do
      SwingRunner.on_edt do
        @pm = PanelController.instance
      end
    end
    it 'should return false for update_on_lqpl_model_trim' do
      SwingRunner.on_edt do
        expect(@pm.update_on_lqpl_model_trim).to be false
      end
    end
  end
end
