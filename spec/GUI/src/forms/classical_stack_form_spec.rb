require 'classical_stack_form'

describe ClassicalStackForm do
  before :each do
    SwingRunner.on_edt do
      @subject = ClassicalStackForm.new
    end
  end
  it 'should not be nil' do
    expect(@subject).not_to be_nil
  end
  it 'delegates the setter to the scrolling label' do
    expect(@subject.the_scrolling_label).to receive(:text=).with('whatever')
    @subject.classical_stack_text = 'whatever'
  end
  it 'delegates the getter to the scrolling label' do
    expect(@subject.the_scrolling_label).to receive(:text).and_return('whatever')
    expect(@subject.classical_stack_text).to eql('whatever')
  end
end
