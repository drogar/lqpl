# Encoding: UTF-8
require 'spec/spec_helper'

describe QuantumStackPanel do
  before :each do
    SwingRunner.on_edt do
      @subject = QuantumStackPanel.new
    end
  end

  it 'should not be nil' do
    expect(@subject).not_to be_nil
  end
end
