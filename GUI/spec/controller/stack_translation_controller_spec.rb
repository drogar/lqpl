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
      @d.set_stack_translation_data(P1)
      @d.get_stack_translation_text.should == "<html><ol><li>p=>1</li></ol></html>"
    end
  end
  
  it "should return false for update_on_lqpl_model_trim" do
    @d.update_on_lqpl_model_trim.should be_false
  end
  it "should return a stack translation model" do
    @d.set_stack_translation_data(P1)
    @d.get_stack_translation.stack_translation.should == [{:p=>1}]
  end
end