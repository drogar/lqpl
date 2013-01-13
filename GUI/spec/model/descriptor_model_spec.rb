require 'spec/spec_helper'

require 'src/panels/quantum_stack/quantum_stack_model'

describe AbstractDescriptorModel do
  it "should only be created by the factory" do
    expect {
      sd = AbstractDescriptorModel.new
    }.to raise_error(ModelCreateError)
  end
  it "should create an instance of ZeroDescriptorModel when created with  <Zero/>" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.class.should == ZeroDescriptorModel
  end
  it "should create an instance of ValueDescriptorModel when created with  <Value>0.5</Value>" do
    sd = AbstractDescriptorModel.make_instance "<Value>0.5</Value>"
    sd.class.should == ValueDescriptorModel
  end

  it "should create an instance of ClassicalDescriptorModel when created with  <Classical><cint>5</cint><cbool>True</cbool></Classical>" do
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>5</cint><cbool>True</cbool></Classical>"
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
    }.to  raise_error(ModelCreateError)
    expect {
      sd = AbstractDescriptorModel.make_instance "ab<Zero/>"
    }.to  raise_error(ModelCreateError)
    expect {
      sd = AbstractDescriptorModel.make_instance "<Alg<Qubit>"
    }.to  raise_error(ModelCreateError)
  end
  it "should embed the incorrect value when raising a create exception" do
    expect {
      sd = AbstractDescriptorModel.make_instance "somethng"
    }.to  raise_error(ModelCreateError, /somethng/)
  end


  it "should return 'nil' when asked for substack labels" do
    sd = AbstractDescriptorModel.make_instance "<Zero/>"
    sd.substack_labels.should be_nil
  end

end




describe ClassicalDescriptorModel do
  it  "should raise an error if constructed with something other than <Classical>list of ints or bools</Classical>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<Classical>err</Classical>"
    }.to raise_error(ParserError, /<Classical>err<\/Classical>/)
  end
  it  "should have a length equal to the number of elements of the passed in list" do
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>14</cint></Classical>"
    sd.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>"
    sd.length.should == 3
  end
  it "should have the value being the list of classicalvalues in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>"
    sd.value.should == [1,true,14]
  end
  it "should return a list of length 'length' when asked for substack labels" do
     sd = AbstractDescriptorModel.make_instance "<Classical><cint>14</cint></Classical>"
    sd.substack_labels.length.should == 1
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>"
    sd.substack_labels.length.should == 3
  end
  it "should have the substack_labels being the list of classicalvalues in the construction string" do
    sd = AbstractDescriptorModel.make_instance "<Classical><cint>1</cint><cbool>True</cbool><cint>14</cint></Classical>"
    sd.substack_labels.should == ["1","true","14"]
  end
  context "class methods" do
    context "validation" do
      it "should not raise an error if passed an array with elements" do
         ClassicalDescriptorModel.validate_substacks_count([1,2]).should be_nil
      end
      it "should raise an error if passed an empty array" do
        expect {
          ClassicalDescriptorModel.validate_substacks_count([])
        }.to raise_error ModelCreateError, /Classical.*should have/
      end
      it "should raise an error if passed a nil array" do
        expect {
          ClassicalDescriptorModel.validate_substacks_count(nil)
        }.to raise_error ModelCreateError, /Classical.*should have/
      end
    end
  end
end



describe DataDescriptorModel do
  it  "should raise an error if constructed with something other than <AlgebraicData>list of pairs</AlgebraicData>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<AlgebraicData>err</AlgebraicData>"
    }.to raise_error(ParserError, /<AlgebraicData>err<\/AlgebraicData>/)
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
  
  context "class methods" do
    context "validation" do
      it "should not raise an error if passed an array with elements" do
          DataDescriptorModel.validate_substacks_count([1,2]).should be_nil
      end
      it "should raise an error if passed an empty array" do
        expect {
          DataDescriptorModel.validate_substacks_count([])
        }.to raise_error ModelCreateError, /Data.*should have/
      end
      it "should raise an error if passed a nil array" do
        expect {
          DataDescriptorModel.validate_substacks_count(nil)
        }.to raise_error ModelCreateError, /Data.*should have/
      end
    end
  end
  
  
end


describe QubitDescriptorModel do
  it  "should raise an error if constructed with something other than <Qubits>list of z,o pairs</Qubits>" do
    expect {
      sc = AbstractDescriptorModel.make_instance "<Qubits>err</Qubits>"
    }.to raise_error(ParserError, /<Qubits>err<\/Qubits>/)
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
  context "class methods" do
    context "validation" do
      it "should not raise an error if passed an array with 1 element" do
          QubitDescriptorModel.validate_substacks_count([1]).should be_nil
      end
      it "should not raise an error if passed an array with 2 elements" do
          QubitDescriptorModel.validate_substacks_count([1,2]).should be_nil
      end
      it "should not raise an error if passed an array with 3 elements" do
          QubitDescriptorModel.validate_substacks_count([1,2,3]).should be_nil
      end
      it "should not raise an error if passed an array with 4 elements" do
          QubitDescriptorModel.validate_substacks_count([1,2,3,4]).should be_nil
      end
      it "should raise an error if passed an empty array" do
        expect {
          QubitDescriptorModel.validate_substacks_count([])
        }.to raise_error ModelCreateError, /Qubit.*should have/
      end
      it "should raise an error if passed a nil array" do
        expect {
          QubitDescriptorModel.validate_substacks_count(nil)
        }.to raise_error ModelCreateError, /Qubit.*should have/
      end
      it "should raise an error if passed an array with > 4 elements" do
        expect {
          QubitDescriptorModel.validate_substacks_count([1,2,3,4,5])
        }.to raise_error ModelCreateError, /Qubit.*should have/
      end
    end
  end
end



describe ValueDescriptorModel do
  it "should successfully be created with input '<Value>0.5</Value>'" do
    z = ValueDescriptorModel.new '<Value>0.5</Value>'
    z.should_not be_nil
  end
  it "should raise an error on other input" do
    expect {
      ValueDescriptorModel.new "<Value><Cint>4</Cint></Value>"
    }.to raise_error ParserError, /Value/
    expect {
      ValueDescriptorModel.new "whatever"
    }.to raise_error ParserError, /whatever/
  end
  context "class methods" do
    context "validation" do
      it "should raise an error if passed an array with elements" do
        expect {
          ValueDescriptorModel.validate_substacks_count([1,2])
        }.to raise_error ModelCreateError, /Value.*should not have/
      end
      it "should not raise an error if passed an empty array" do
          ValueDescriptorModel.validate_substacks_count([]).should be_nil
      end
      it "should not raise an error if passed an array with no elements" do
          ValueDescriptorModel.validate_substacks_count(nil).should be_nil
      end
    end
  end
  context 'attributes' do
    it "should always have a length of 0" do
      sd = AbstractDescriptorModel.make_instance "<Value>0.5</Value>"
      sd.length.should == 0
    end
    it "should have the value in the construction string" do
      sd = AbstractDescriptorModel.make_instance "<Value>6.25e-2</Value>"
      sd.value.should == 6.25e-2
    end
    it "should allow a number tag to surround the data" do
      sd = AbstractDescriptorModel.make_instance "<Value><number>0.32</number></Value>"
      sd.value.should == 0.32
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
end


describe ZeroDescriptorModel do
  it "Should create when given no input" do
    z = ZeroDescriptorModel.new
    z.should_not be_nil
  end
  it "should successfully be created with input '<Zero/>'" do
    z = ZeroDescriptorModel.new '<Zero/>'
    z.should_not be_nil
  end
  it "should raise an error on other input" do
    expect {
      ZeroDescriptorModel.new "<Value><Cint>4</Cint></Value>"
    }.to raise_error ParserError, /Value/
    expect {
      ZeroDescriptorModel.new "whatever"
    }.to raise_error ParserError, /whatever/
  end
  context "class methods" do
    context "validation" do
      it "should raise an error if passed an array with elements" do
        expect {
          ZeroDescriptorModel.validate_substacks_count([1,2])
        }.to raise_error ModelCreateError, /Zero.*should not have/
      end
      it "should not raise an error if passed an empty array" do
          ZeroDescriptorModel.validate_substacks_count([]).should be_nil
      end
      it "should not raise an error if passed an array with no elements" do
          ZeroDescriptorModel.validate_substacks_count(nil).should be_nil
      end
    end
  end
  context "attributes" do
    before(:each) do
      @z = ZeroDescriptorModel.new
    end
    it "should have a value of '0'" do
      @z.value.should == "0"
    end
    it "should have a length of 0" do
      @z.length.should == 0
    end
    it "should have no name" do
      @z.name.should be_nil
    end
    it "should return 'nil' when asked for substack labels" do
      @z.substack_labels.should be_nil
    end
 end
end

