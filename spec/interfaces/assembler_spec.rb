require 'spec/spec_helper'

describe Assembler do
  it "connects to the assembler process when created"
  it "generates an error if there is no assembler process"
  it "reads data from a QPL file when given a filename"
  it "sends QPL code to the assembler process"
  it "recieves QPO code from the assembler process"
end