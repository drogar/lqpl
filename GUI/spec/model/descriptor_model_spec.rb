require 'spec/spec_helper'

require 'src/panels/quantum_stack/quantum_stack_model'

describe AbstractDescriptorModel do
  it "should only be created by the factory" do
    expect {
      sd = AbstractDescriptorModel.new
    }.to raise_error(StackDescriptorModelInvalidCreate)
  end
  it "should create an instance of ZeroDescriptorModel when created with  <Zero/>" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.class.should == ZeroDescriptorModel
  end
  it "should create an instance of ValueDescriptorModel when created with  <Value>0.5</Value>" do
    sd = AbstractDescriptorModel.make_instance "<Value>0.5</Value>"
    sd.class.should == ValueDescriptorModel
  end

  it "should create an instance of ClassicalDescriptorModel when created with  <ClassicalStack><cint>5</cint><cbool>True</cbool></ClassicalStack>" do
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>5</cint><cbool>True</cbool></ClassicalStack>"
    sd.class.should == ClassicalDescriptorModel
  end
  it "should create an instance of QubitDescriptorModel when created with  <Qubits><pair><qz/><qz/></pair></Qubits>" do
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sd.class.should == QubitDescriptorModel
  end
  it "should create an instance of DataDescriptorModel when created with  <AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.class.should == DataDescriptorModel
  end
  it "should raise an error when the input string to the factory doesn't start with one of <Zero, <Value, <Class, <Qubi, or <Alge" do
    expect {
      sd = AbstractDescriptorModel.make_instance "somethng"
    }.to  raise_error(StackDescriptorModelInvalidCreate)
    expect {
      sd = AbstractDescriptorModel.make_instance "ab<Zero/>"
    }.to  raise_error(StackDescriptorModelInvalidCreate)
    expect {
      sd = AbstractDescriptorModel.make_instance "<Alg<Qubit>"
    }.to  raise_error(StackDescriptorModelInvalidCreate)
  end
  it "should embed the incorrect value when raising a create exception" do
    expect {
      sd = AbstractDescriptorModel.make_instance "somethng"
    }.to  raise_error(StackDescriptorModelInvalidCreate, /somethng/)
  end


  it "should return 'nil' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.substack_labels.should be_nil
  end

end




describe ClassicalDescriptorModel do
  it  "should raise an error if constructed with something other than <ClassicalStack>list of ints or bools</ClassicalStack>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<ClassicalStack>err</ClassicalStack>"
    }.to raise_error(StackDescriptorModelInvalidCreate, "<ClassicalStack>err</ClassicalStack>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>14</cint></ClassicalStack>"
    sd.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.length.should == 3
  end
  it "should have the value being the list of classicalvalues in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.value.should == [1,true,14]
  end
  it "should return a list of length 'length' when asked for substack labels" do
     sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>14</cint></ClassicalStack>"
    sd.substack_labels.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.substack_labels.length.should == 3
  end
  it "should have the substack_labels being the list of classicalvalues in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<ClassicalStack><cint>1</cint><cbool>True</cbool><cint>14</cint></ClassicalStack>"
    sd.substack_labels.should == ["1","true","14"]
  end

  context "list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        ClassicalDescriptorModel::parse_list("invalid")
      }.to raise_error(StackDescriptorModelInvalidCreate, "invalid")
    end
    it "should prepare a list of length 0 when there is no list" do
      ClassicalDescriptorModel::parse_list("").should == []
    end
    it "should result in a one element int list when that is the only input" do
      ClassicalDescriptorModel::parse_list("<cint>1</cint>").should == [1]
      ClassicalDescriptorModel::parse_list("<cint>-3491</cint>").should == [-3491]
    end
    it "should result in a one element bool list when that is the only input" do
      ClassicalDescriptorModel::parse_list("<cbool>True</cbool>").should == [true]
      ClassicalDescriptorModel::parse_list("<cbool>False</cbool>").should == [false]
    end
    it "should result in a mixed list when that is the same input" do
      ClassicalDescriptorModel::parse_list("<cint>1</cint><cbool>True</cbool><cint>14</cint>").should == [1,true,14]
    end
  end
end



describe DataDescriptorModel do
  it  "should raise an error if constructed with something other than <AlgebraicData>list of z,o pairs</AlgebraicData>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<AlgebraicData>err</AlgebraicData>"
    }.to raise_error(StackDescriptorModelInvalidCreate, "<AlgebraicData>err</AlgebraicData>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>Nil2</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.length.should == 2
  end
  it "should have the value being the map of constructor/address pairs in the string" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses></AlgebraicData>"
    sd.value.should == [["Nil",[]], ["C",[3,4]]]
  end
  it "should return a list of length 'length' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.substack_labels.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>Nil2</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.substack_labels.length.should == 2
  end

  it "should have constructer names only when there are no stack addresses" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.substack_labels.should == ["Nil"]
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>Nil2</string><StackAddresses></StackAddresses></AlgebraicData>"
    sd.substack_labels.should == ["Nil", "Nil2"]
  end

  it "should have the substack_labels being the list of constructors with addresses in brackets" do
    sd = AbstractDescriptorModel.make_instance "<AlgebraicData><string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses></AlgebraicData>"
    sd.substack_labels.should == ["Nil", "C[3, 4]"]
  end
  context "constructor matching" do
    it "should raise an error when the constructor map is invalid" do
      expect {
        DataDescriptorModel::parse_pairs("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should raise an error when there is no constructor" do
      expect {
        DataDescriptorModel::parse_pairs("")
      }.to raise_error(InvalidInput, "Must have at least one constructor")
    end
    it "should result in a one element array of pairs when that is the only input" do
      DataDescriptorModel::parse_pairs("<string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses>").should == [["C",[3,4]]]
    end
    it "should result in a mixed map when that is the input" do
      DataDescriptorModel::parse_pairs("<string>Nil</string><StackAddresses></StackAddresses><string>C</string><StackAddresses><int>3</int><int>4</int></StackAddresses>").should ==
       [["Nil",[]], ["C" , [3,4]]]
    end
    it "should raise an error when the map has duplicated constructors" do
      expect {
        DataDescriptorModel::parse_pairs("<string>Nil</string><StackAddresses></StackAddresses><string>Nil</string><StackAddresses></StackAddresses>")
      }.to raise_error(InvalidInput, "Constructor 'Nil' duplicated in algebraic data")
    end
  end
  context "address list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        DataDescriptorModel::parse_address_list("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should result in an empty list when there is no input" do
      DataDescriptorModel::parse_address_list("").should ==  []
    end
    it "should result in a one element list when that is the only input" do
      DataDescriptorModel::parse_address_list("<int>3</int>").should ==  [3]
    end
    it "should result in a many element list when that is the input" do
      DataDescriptorModel::parse_address_list("<int>3</int><int>4</int><int>5</int><int>6</int><int>7</int>").should ==  [3,4,5,6,7]
    end
    it "should raise an error when the list has duplicated elements" do
      expect {
        DataDescriptorModel::parse_address_list("<int>3</int><int>3</int>")
      }.to raise_error(InvalidInput, "StackAddress '3' duplicated for single constructor")
    end
  end
end


describe QubitDescriptorModel do
  it  "should raise an error if constructed with something other than <Qubits>list of z,o pairs</Qubits>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<Qubits>err</Qubits>"
    }.to raise_error(StackDescriptorModelInvalidCreate, "<Qubits>err</Qubits>")
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sd.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.length.should == 2
  end
  it "should have the value being the list of qubit indicators in the string" do
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.value.should == [[0,0],[0,1],[1,0],[1,1]]
  end
  it "should return a list of length 'length' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair></Qubits>"
    sd.substack_labels.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.substack_labels.length.should == 2
  end
  it "should have the substack_labels being the list of 01 pairs in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits>"
    sd.substack_labels.should == ["00","01","10","11"]
  end
  context "list matching" do
    it "should raise an error when the list is invalid" do
      expect {
        QubitDescriptorModel::parse_list("invalid")
      }.to raise_error(InvalidInput, "invalid")
    end
    it "should raise and error when there is no list" do
      expect {
        QubitDescriptorModel::parse_list("")
      }.to raise_error(InvalidInput, "Must have qubit indicators")
    end
    it "should result in a one element pair list when that is the only input" do
      QubitDescriptorModel::parse_list("<pair><qz/><qz/></pair>").should == [[0,0]]
    end
    it "should result in a mixed list when that is the same input" do
      QubitDescriptorModel::parse_list("<pair><qz/><qz/></pair><pair><qz/><qo/></pair>").should == [[0,0],[0,1]]
    end
    it "should raise an error when the list has duplicated pairs" do
      expect {
        QubitDescriptorModel::parse_list("<pair><qz/><qz/></pair><pair><qz/><qz/></pair>")
      }.to raise_error(InvalidInput, "[0, 0] duplicated in qubit")
    end
  end
  context "translate qubit" do
    it "should return 1 for <qo/>" do
      QubitDescriptorModel::translate_qubit("<qo/>").should == 1
    end
    it "should return 0 for <qz/>" do
      QubitDescriptorModel::translate_qubit("<qz/>").should == 0
    end
    it "should raise an exception for any other input" do
      expect {
        QubitDescriptorModel::translate_qubit("err")
      }.to raise_error InvalidInput, /err/
    end
  end
end



describe ValueDescriptorModel do
  it "should always have a length of 0" do
    sd = AbstractDescriptorModel.make_instance "<Value>0.5</Value>"
    sd.length.should == 0
  end

  it "should have the value in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<Value>6.25e-2</Value>"
    sd.value.should == "6.25e-2"
  end
  it "should allow a number tag to surround the data" do
    sd = AbstractDescriptorModel.make_instance "<Value><number>0.32</number></Value>"
    sd.value.should == "0.32"
  end
  it "should have no name" do
    sd = AbstractDescriptorModel.make_instance "<Value>0.5</Value>"
    sd.name.should be_nil
  end


  it "should return 'nil' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<Value><number>0.32</number></Value>"
    sd.substack_labels.should be_nil
  end
end


describe ZeroDescriptorModel do
  it "should always have a length of 0" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.length.should == 0
  end
  it "should raise an error if constructed with something other than <Zero/>" do
    expect {
      sd = AbstractDescriptorModel.make_instance "<Zero>"
    }.to raise_error(StackDescriptorModelInvalidCreate, "<Zero>")
    expect {
      sd = ZeroDescriptorModel.new "wrong"
    }.to raise_error(StackDescriptorModelInvalidCreate, "wrong")
  end
  it "should have no name" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.name.should be_nil
  end

  it "should return 'nil' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.substack_labels.should be_nil
  end

end

