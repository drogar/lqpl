require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'
require 'src/panels/stack_translation/stack_translation_controller'
require 'src/panels/stack_translation/stack_translation_model'

describe StackTranslationController do
  before(:each) do
    @d = StackTranslationController.instance
  end
  it "should accessibly store the dump text in the model" do
    @d.set_stack_translation_data(P1)
    @d.get_stack_translation_text.should == "<html><ol><li>p=>1</li></ol></html>"
  end

  it "should set the server_connection when given an sc" do

    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    @d.lqpl_emulator_server_connection=sc
  end
  it "should ask the sc for the stack translation when given a depth and recursion" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    sc.should_receive(:get_stack_translation).and_return(P1)

    @d.lqpl_emulator_server_connection=sc
    @d.set_stack_translation("5","4")
  end
  it "should return a stack translation model" do
    @d.set_stack_translation_data(P1)
    @d.get_stack_translation.stack_translation.should == [{:p=>1}]
  end
end