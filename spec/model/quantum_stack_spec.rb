require 'spec/spec_helper'

describe QuantumStack do
  it "should give an invalid create error when created with incorrect data" do
    expect {
      qs = QuantumStack.new "err"
    }.to raise_error QuantumStackInvalidCreate, /err/
  end
  it "should give an error when stackzero has substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks><bottom/></substacks><Zero/></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /not have/
  end
  it "should give an error when stackvalue has substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks><bottom/></substacks><Value>0.5</Value></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /not have/
  end
  it "should give an error when stackqubit does not have substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should give an error when stackclassical does not have substacks"  do
    expect {
      qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks></substacks><ClassicalStack><cint>14</cint></ClassicalStack></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should give an error when stackdata does not have substacks" do
    expect {
      qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"
    }.to raise_error QuantumStackInvalidCreate, /should have/
  end
  it "should allow 'bottom' as the construction"  do
    qs = QuantumStack.new "<bottom/>"
    qs.should be_bottom
  end
  it "should allow 'bottom' in place of substacks"  do
    qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
  end
  it "should have the same number of substacks as the length of the descriptor" do
    qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
    qs.substacks.length.should == 1
    qs = QuantumStack.new "<Qstack><StackAddress><int>1</int></StackAddress><bool>True</bool><substacks><bottom/><bottom/><bottom/></substacks><ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack></Qstack>"
    qs.substacks.length.should == 3

  end
end