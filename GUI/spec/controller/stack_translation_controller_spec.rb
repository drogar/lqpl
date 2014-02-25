require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'
require 'src/panels/stack_translation/stack_translation_controller'
require 'src/panels/stack_translation/stack_translation_model'

describe StackTranslationController do
  before(:each) do
    SwingRunner::on_edt do
      @d = StackTranslationController.instance
    end
  end
  it "should accessibly store the dump text in the model" do
    SwingRunner::on_edt do
      @d.update_stack_translation_data(P1)
      expect(@d.stack_translation_text).to eq("<html><ol><li>p=>1</li></ol></html>")
    end
  end

  it "should return false for update_on_lqpl_model_trim" do
    expect(@d.update_on_lqpl_model_trim).to be_false
  end
  it "should return a stack translation model" do
    @d.update_stack_translation_data(P1)
    expect(@d.stack_translation.stack_translation).to eq([{:p=>1}])
  end
end