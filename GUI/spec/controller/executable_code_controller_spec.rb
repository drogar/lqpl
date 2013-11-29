require 'spec/spec_helper'


describe ExecutableCodeController do
  before(:each) do
    SwingRunner::on_edt do
      @c = ExecutableCodeController.instance
    end
  end
  specify {expect(@c.update_on_lqpl_model_trim).to be_false}
end