# encoding: UTF-8
require 'spec/spec_helper'

describe QuantumStackForm do
  before :each do
    SwingRunner.on_edt do
      @qsf = QuantumStackForm.new
    end
  end
  it 'should not be nil' do
    expect(@qsf).not_to be_nil
  end
end
