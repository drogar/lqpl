require 'spec/spec_helper'

describe StackDescriptor do
  it "should only be created by the factory" do
    expect {
      sd = StackDescriptor.new
    }.to raise_error(StackDescriptorInvalidCreate)
  end
  it "should create an instance of StackZero when created with  <Zero/>" do
    sd = StackDescriptor.make_instance "<Zero/>"
    sd.class.should == StackZero
  end
  it "should create an instance of StackValue when created with  <Value>0.5</Value>" do
    sd = StackDescriptor.make_instance "<Value>0.5</Value>"
    sd.class.should == StackValue
  end

  it "should create an instance of StackClassical when created with  <ClassicalStack><cint>5</cint><cbool>True</cbool></ClassicalStack>" do
    sd = StackDescriptor.make_instance "<ClassicalStack><cint>5</cint><cbool>True</cbool></ClassicalStack>"
    sd.class.should == StackClassical
  end
  it "should create an instance of StackQubit when created with  <Qubits><pair><qz/><qz/></pair></Qubits>" do
    sd = StackDescriptor.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sd.class.should == StackQubit
  end
  it "should create an instance of StackData when created with  <AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>" do
    sd = StackDescriptor.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.class.should == StackData
  end
  it "should raise an error when the input string to the factory doesn't start with one of <Zero, <Value, <Class, <Qubi, or <Alge" do
    expect {
      sd = StackDescriptor.make_instance "somethng"
    }.to  raise_error(StackDescriptorInvalidCreate)
    expect {
      sd = StackDescriptor.make_instance "ab<Zero/>"
    }.to  raise_error(StackDescriptorInvalidCreate)
    expect {
      sd = StackDescriptor.make_instance "<Alg<Qubit>"
    }.to  raise_error(StackDescriptorInvalidCreate)
  end
  it "should embed the incorrect value when raising a create exception" do
    expect {
      sd = StackDescriptor.make_instance "somethng"
    }.to  raise_error(StackDescriptorInvalidCreate, /somethng/)
  end
end

describe StackZero do
  it "should always have a length of 0" do
    sd = StackDescriptor.make_instance "<Zero/>"
    sd.length.should == 0
  end
  it "should raise an error if constructed with something other than <Zero/>" do
    expect {
      sd = StackDescriptor.make_instance "<Zero>"
    }.to raise_error(StackDescriptorInvalidCreate, "<Zero>")
    expect {
      sd = StackZero.new "wrong"
    }.to raise_error(StackDescriptorInvalidCreate, "wrong")
  end

end

describe StackValue do
  it "should always have a length of 0" do
    sd = StackDescriptor.make_instance "<Value>0.5</Value>"
    sd.length.should == 0
  end
  it "should raise an error if constructed with something other than <Value>fnumber</Value>" do
    expect {
      sd = StackDescriptor.make_instance "<Value>err</Value>"
    }.to raise_error(StackDescriptorInvalidCreate, "<Value>err</Value>")
  end
  it "should raise an error if constructed with a value <0" do
    expect {
      sd = StackDescriptor.make_instance "<Value>-0.01</Value>"
    }.to raise_error(StackDescriptorInvalidCreate, "<Value>-0.01</Value>")
  end
  it "should raise an error if constructed with a value >1" do
    expect {
      sd = StackDescriptor.make_instance "<Value>1.01</Value>"
    }.to raise_error(StackDescriptorInvalidCreate, "<Value>1.01</Value>")
  end
end

