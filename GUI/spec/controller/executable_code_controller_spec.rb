require 'spec/spec_helper'


describe ExecutableCodeController do
  before(:each) do
    @c = ExecutableCodeController.instance
  end
  it "should return false for update_on_lqpl_model_trim" do
    @c.update_on_lqpl_model_trim.should be_false
  end
end