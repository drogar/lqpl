require 'spec/spec_helper'


describe ExecutableCodeController do
  before(:each) do
    SwingRunner::on_edt do
      @c = ExecutableCodeController.instance
    end
  end
  specify {@c.update_on_lqpl_model_trim.should be_false}
end