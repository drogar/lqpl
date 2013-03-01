require 'spec/spec_helper'

describe QuantumStackForm do
  before (:each) do
    SwingRunner::on_edt do
      @qsf = QuantumStackForm.new
    end
  end
  it "should not be nil" do
    @qsf.should_not be_nil
  end
end