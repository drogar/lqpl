# encoding: UTF-8
require 'spec/spec_helper'

describe AboutDialog do
  before :each do
    SwingRunner.on_edt do
      @ab = AboutDialog.new
    end
  end

  it 'should not be nil' do
    expect(@ab).not_to be_nil
  end

  it "should have a title of 'About LQPL'" do
    expect(@ab.edt_title).to eq('About LQPL')
  end

  specify { expect(@ab.data_pane.components.size).to eql(1) }

  context 'the text label' do
    before :each do
      @tp = @ab.data_pane.components[0]
    end
    specify { expect(@tp.class).to eq(Panel) }
    specify { expect(@tp.components.size).to eql(1) }
    specify { expect(@tp.components[0].class).to eq(Label) }
    context 'the label' do
      before :each do
        @lab = @tp.components[0]
      end
      specify { expect(@ab.about_data_label).to eq(@lab) }
    end
  end
end
