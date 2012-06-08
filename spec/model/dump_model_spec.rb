require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'src/panels/dump/dump_model'

describe DumpModel do
  it "should accessibly store the dump text in the model" do
    d = DumpModel.new
    d.dump_text = "junk"
    d.dump_text.should == "junk"
  end
end