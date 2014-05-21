# encoding: UTF-8
require 'spec/spec_helper'

describe ExecutingCodeController do
  before(:each) do
    SwingRunner.on_edt do
      @c = ExecutingCodeController.instance
    end
  end
  specify { expect(@c.update_on_lqpl_model_trim).to be false }
end
