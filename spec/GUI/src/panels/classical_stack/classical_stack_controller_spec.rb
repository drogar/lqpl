require 'classical_stack_controller'
require 'classical_stack_model'

describe ClassicalStackController do
  before(:each) do
    SwingRunner.on_edt do
      @c = ClassicalStackController.instance
    end
  end
  it 'should raise an error when created with junk' do
    expect { @c.update_classical_stack_data('junk') }. to raise_error JSON::ParserError, /junk/
  end

  specify { expect(@c.update_on_lqpl_model_trim).to be false }
  it 'should create a classical stack when given the correct input' do
    SwingRunner.on_edt do
      @c.update_classical_stack_data('{"cstack":[-27, true, 40, false]}')
      expect(@c.classical_stack_data).to eq('<html>-27<br />true<br />40<br />false</html>')
    end
  end
end
