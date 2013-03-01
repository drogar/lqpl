require 'spec/spec_helper'



describe QuantumStackView do
  before (:each) do
    
    SwingRunner::on_edt do
      @qsv = QuantumStackView.new
    end
  end
  it "should not be nil" do
    @qsv.should_not be_nil
  end
end