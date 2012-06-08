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
    @d.get_stack_translation_text.should == "[{:p=>1}]"
  end

  it "should set the server_connection when given an sc" do

    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    @d.server_connection=sc
  end
  it "should ask the sc for the dump when given a depth and recursion" do
    sc = double('server_connection')
    sc.should_receive(:connected?).and_return(true)
    sc.should_receive(:get_stack_translation).and_return(P1)

    @d.server_connection=sc
    @d.set_stack_translation("5","4")
  end
end