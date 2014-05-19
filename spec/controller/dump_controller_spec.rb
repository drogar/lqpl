require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'GUI/src/panels/dump/dump_controller'
require 'GUI/src/panels/dump/dump_model'

describe DumpController do
  before(:each) do
    SwingRunner::on_edt do
      @c = DumpController.instance
    end
  end
  it 'should accessibly store the dump text in the model' do
    SwingRunner::on_edt do
      @c.update_dump_data(DUMPSINGLECALL)
      expect(@c.dump_data).to eq('<html><ol><li>Return to Ret(3). CS=[]</li></ol></html>')
    end
  end

  specify {expect(@c.update_on_lqpl_model_trim).to be true}
end
