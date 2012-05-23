require 'spec/spec_helper'

java_import java.awt.image.BufferedImage

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
  it "should have no name" do
    sd = StackDescriptor.make_instance "<Zero/>"
    sd.name.should be_nil
  end
  it "should have a preferred size of W=10, H > 15" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    sd = StackDescriptor.make_instance "<Zero/>"
    sd.get_preferred_size(g).width.should == 10
    sd.get_preferred_size(g).height.should > 15
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

  it "should have the value in the construction string" do
    sd = StackDescriptor.make_instance "<Value>0.32</Value>"
    sd.value.should == "0.32"
  end
  it "should allow a number tag to surround the data" do
    sd = StackDescriptor.make_instance "<Value><number>0.32</number></Value>"
    sd.value.should == "0.32"
  end
  it "should have no name" do
    sd = StackDescriptor.make_instance "<Value>0.5</Value>"
    sd.name.should be_nil
  end
  it "should have a preferred size of W=10, H > 15" do
    g = BufferedImage.new(500,500,BufferedImage::TYPE_INT_RGB).graphics
    sd = StackDescriptor.make_instance "<Value>0.5</Value>"
    sd.get_preferred_size(g).width.should == 10
    sd.get_preferred_size(g).height.should > 15
  end
end

describe StackClassical do
  it  "should raise an error if constructed with something other than <ClassicalStack>list of ints or bools</ClassicalStack>" do
    expect {
      sc = StackDescriptor.make_instance "<ClassicalStack>err</ClassicalStack>"
    }.to raise_error(StackDescriptorInvalidCreate, "<ClassicalStack>err</ClassicalStack>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = StackDescriptor.make_instance "<ClassicalStack><cint>14</cint></ClassicalStack>"
    sd.length.should == 1
    sd = StackDescriptor.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.length.should == 3
  end
  it "should have the value being the list of classicalvalues in the construction string" do
    sd = StackDescriptor.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.value.should == [1,true,14]
  end
  context "list matching" do
    before (:each) do
      @sd = StackDescriptor.make_instance "<ClassicalStack></ClassicalStack>"
    end
    it "should raise an error when the list is invalid" do
      expect {
        @sd.parse_list("invalid")
      }.to raise_error(StackDescriptorInvalidCreate, "invalid")
    end
    it "should prepare a list of length 0 when there is no list" do
      @sd.parse_list("").should == []
    end
    it "should result in a one element int list when that is the only input" do
      @sd.parse_list("<cint>1</cint>").should == [1]
      @sd.parse_list("<cint>-3491</cint>").should == [-3491]
    end
    it "should result in a one element bool list when that is the only input" do
      @sd.parse_list("<cbool>True</cbool>").should == [true]
      @sd.parse_list("<cbool>False</cbool>").should == [false]
    end
    it "should result in a mixed list when that is the same input" do
      @sd.parse_list("<cint>1</cint><cbool>True</cbool><cint>14</cint>").should == [1,true,14]
    end
  end
end

describe StackQubit do
  it  "should raise an error if constructed with something other than <Qubits>list of z,o pairs</Qubits>" do
    expect {
      sc = StackDescriptor.make_instance "<Qubits>err</Qubits>"
    }.to raise_error(StackDescriptorInvalidCreate, "<Qubits>err</Qubits>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = StackDescriptor.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sd.length.should == 1
    sd = StackDescriptor.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.length.should == 2
  end
  it "should have the value being the list of qubit indicators in the string" do
    sd = StackDescriptor.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.value.should == [[0,0],[0,1],[1,0],[1,1]]
  end
  context "list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        StackQubit::parse_list("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should raise and error when there is no list" do
      expect {
        StackQubit::parse_list("")
      }.to raise_error(InvalidInput, "Must have qubit indicators")
    end
    it "should result in a one element pair list when that is the only input" do
      StackQubit::parse_list("<pair><qz/><qz/></pair>").should == [[0,0]]
    end
    it "should result in a mixed list when that is the same input" do
      StackQubit::parse_list("<pair><qz/><qz/></pair><pair><qz/><qo/></pair>").should == [[0,0],[0,1]]
    end
    it "should raise an error when the list has duplicated pairs" do
      expect {
        StackQubit::parse_list("<pair><qz/><qz/></pair><pair><qz/><qz/></pair>")
      }.to raise_error(InvalidInput, "[0, 0] duplicated in qubit")
    end
  end
  context "translate qubit" do
    it "should return 1 for <qo/>" do
      StackQubit::translate_qubit("<qo/>").should == 1
    end
    it "should return 0 for <qz/>" do
      StackQubit::translate_qubit("<qz/>").should == 0
    end
    it "should raise an exception for any other input" do
      expect {
        StackQubit::translate_qubit("err")
      }.to raise_error InvalidInput, /err/
    end
  end
end


describe StackData do
  it  "should raise an error if constructed with something other than <AlgebraicData>list of z,o pairs</AlgebraicData>" do
    expect {
      sc = StackDescriptor.make_instance "<AlgebraicData>err</AlgebraicData>"
    }.to raise_error(StackDescriptorInvalidCreate, "<AlgebraicData>err</AlgebraicData>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = StackDescriptor.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.length.should == 1
    sd = StackDescriptor.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>Nil2</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.length.should == 2
  end
  it "should have the value being the map of constructor/address pairs in the string" do
    sd = StackDescriptor.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses></AlgebraicData>"
    sd.value.should == {"Nil"=>[], "C" => [3,4]}
  end
  context "constructor matching" do
    it "should raise an error when the constructor map is invalid" do
      expect {
        StackData::parse_map("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should raise an error when there is no constructor" do
      expect {
        StackData::parse_map("")
      }.to raise_error(InvalidInput, "Must have at least one constructor")
    end
    it "should result in a one element map when that is the only input" do
      StackData::parse_map("<string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses>").should == {"C" => [3,4]}
    end
    it "should result in a mixed map when that is the input" do
      StackData::parse_map("<string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses>").should ==
       {"Nil"=>[], "C" => [3,4]}
    end
    it "should raise an error when the map has duplicated constructors" do
      expect {
        StackData::parse_map("<string>Nil</string><StackAddresses></StackAddresses><string>Nil</string><StackAddresses></StackAddresses>")
      }.to raise_error(InvalidInput, "Constructor 'Nil' duplicated in algebraic data")
    end
  end
  context "address list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        StackData::parse_address_list("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should result in an empty list when there is no input" do
      StackData::parse_address_list("").should ==  []
    end
    it "should result in a one element list when that is the only input" do
      StackData::parse_address_list("<int>3</int>").should ==  [3]
    end
    it "should result in a many element list when that is the input" do
      StackData::parse_address_list("<int>3</int><int>4</int><int>5</int><int>6</int><int>7</int>").should ==  [3,4,5,6,7]
    end
    it "should raise an error when the list has duplicated elements" do
      expect {
        StackData::parse_address_list("<int>3</int><int>3</int>")
      }.to raise_error(InvalidInput, "StackAddress '3' duplicated for single constructor")
    end
  end
end


