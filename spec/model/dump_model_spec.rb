# Encoding: UTF-8
require 'spec/spec_helper'

require 'spec/specdata/dump_data'
require 'GUI/src/panels/dump/dump_model'

describe DumpModel do
  subject { DumpModel.new }
  it 'should accessibly store the dumpcall data in the model' do
    subject.dump = DUMPSINGLECALL
    expect(subject.text).to match(/Return to Ret\(3\)\. CS=\[\]/)
  end
  it 'should just accept and display the text of the split' do
    subject.dump = DUMPSPLIT
    expect(subject.text).to match(/qsresult/)
  end
  it 'should create a list of dumpcall and dumpsplit items' do
    subject.dump = DUMPCALLSPLITCALL
    expect(subject.dump.length).to eql(3)
    expect(subject.text).to match(/Ret\(3\)/)
    expect(subject.text).to match(/some_method\(7\)/)
    expect(subject.text).to match(/qsresult/)
  end
end
