# Encoding: UTF-8
require 'spec/spec_helper'

describe DumpForm do
  before :each do
    SwingRunner.on_edt do
      @subject = DumpForm.new
    end
  end
  it 'should not be nil' do
    expect(@subject).not_to be_nil
  end
  it 'delegates the setter to the scrolling label' do
    expect(@subject.the_scrolling_label).to receive(:text=).with('whatever')
    @subject.dump_text = 'whatever'
  end
  it 'delegates the getter to the scrolling label' do
    expect(@subject.the_scrolling_label).to receive(:text).and_return('whatever')
    expect(@subject.dump_text).to eql('whatever')
  end
end
