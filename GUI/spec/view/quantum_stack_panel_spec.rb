require 'spec/spec_helper'

describe QuantumStackPanel do
  before (:each) do
    
    SwingRunner::on_edt do
      @qsp = QuantumStackPanel.new
    end
  end
  it "should not be nil" do
    @qsp.should_not be_nil
  end
end