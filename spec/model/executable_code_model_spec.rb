require 'spec/spec_helper'
require 'src/panels/executable_code/executable_code_model'

KVPS6='<i>EnScope</i><i>QLoad "@q" 0</i><i>QApply 0 Hadamard "@q"</i>'+
  '<i>QPullup "@q"</i><i>EnScope</i><i>Measure "@q" 14 6 10</i>'

KVPAIRS_2 = '<kvpair><key><string>main</string></key>'+
        '<value><instructions><i>EnScope</i></instructions></value></kvpair>'+
        '<kvpair><key><string>cflip_fcdelbl0</string></key>'+
        '<value><instructions><i>EnScope</i><i>QLoad "@q" 0</i></instructions></value></kvpair>'

CMAP_2='<Code><map>'+KVPAIRS_2+'</map></Code>'

RES_CMAP_2 = {:main => ["  0  EnScope"], :cflip_fcdelbl0 => ["  0  EnScope", '  1  QLoad "@q" 0']}

describe ExecutableCodeModel do
  describe "class method instructions_to_list" do
    it "should make an empty list when no pairs" do
      ExecutableCodeModel::instructions_to_list("").should == []
    end
    it "should make a singleton list when there is one <i> and </i> pair and prepend '  0  ' to the item" do
      ExecutableCodeModel::instructions_to_list("<i>EnScope</i>").should == ["  0  EnScope"]
    end
    it "should make a list of all items between <i> and </i> pairs and prepend the index of the item " do
      ExecutableCodeModel::instructions_to_list(KVPS6).should == ['  0  EnScope',
        '  1  QLoad "@q" 0',
        '  2  QApply 0 Hadamard "@q"',
        '  3  QPullup "@q"',
        '  4  EnScope',
        '  5  Measure "@q" 14 6 10']
    end
    it "should properly justify indexes when the index > 0 " do
      ExecutableCodeModel::instructions_to_list(KVPS6+KVPS6)[9,11].should == [
        '  9  QPullup "@q"',
        ' 10  EnScope',
        ' 11  Measure "@q" 14 6 10']
    end
  end
  describe "class method code_xml_to_map" do
    it "should return nil if input is not valid" do
      ExecutableCodeModel::code_xml_to_map('<junk>').should be_nil
    end
    it "prepares an empty map when there are no keys" do
      ExecutableCodeModel::code_xml_to_map('<Code><map></map></Code>').should == {}
    end
    it "prepares an one element map when there is only one key in the XML" do
      ExecutableCodeModel::code_xml_to_map('<Code><map><kvpair><key><string>main</string></key>'+
        '<value><instructions><i>EnScope</i></instructions></value></kvpair></map></Code>').should == {:main => ["  0  EnScope"]}
    end
    it "prepares an two element map when there are two keys in the XML" do
      ExecutableCodeModel::code_xml_to_map(CMAP_2).should ==RES_CMAP_2
    end
  end
  describe "class method kv_pairs_to_map" do
    it "prepares an empty map when there are no keys" do
      ExecutableCodeModel::kv_pairs_to_map('').should == {}
    end
    it "prepares an one element map when there is only one key in the XML" do
      ExecutableCodeModel::kv_pairs_to_map('<kvpair><key><string>main</string></key>'+
        '<value><instructions><i>EnScope</i></instructions></value></kvpair>').should == {:main => ["  0  EnScope"]}
    end
    it "prepares an two element map when there are two keys in the XML" do
      ExecutableCodeModel::kv_pairs_to_map(KVPAIRS_2).should == RES_CMAP_2
    end
  end
  describe "class method code_pointer_xml_to_map" do
    it "should return an empty map with no or nil input" do
      ExecutableCodeModel::code_pointer_xml_to_map(nil).should == {}
      ExecutableCodeModel::code_pointer_xml_to_map('').should == {}
    end

    it "should return an one element map with a pair input" do
      ExecutableCodeModel::code_pointer_xml_to_map("<pair><string>main</string><int>2</int></pair>").should == {:main => 2}
    end

    it "should return nil with invalid input" do
      ExecutableCodeModel::code_pointer_xml_to_map("junk").should be_nil
    end
  end
  describe "public instance methods" do

    describe "the_code" do
      before(:each) do
        @ecm = ExecutableCodeModel.new
      end
      it "should throw an exception with bad input" do
        expect {@ecm.the_code="junk"}.to raise_error QuantumStackModelInvalidCreate, /code xml/
      end
      it "should return the created code map when given correct input" do
        @ecm.the_code=CMAP_2
        @ecm.the_code.should == RES_CMAP_2
      end
      it "should return the nil by default" do
        @ecm.the_code.should be_nil
      end
    end
    describe "the_code_pointer" do
      before(:each) do
        @ecm = ExecutableCodeModel.new
      end
      it "should throw an exception with bad input" do
        expect {@ecm.the_code_pointer="junk"}.to raise_error QuantumStackModelInvalidCreate, /code pointer xml/
      end
      it "should return the created code map when given correct input when there is code" do
        @ecm.the_code=CMAP_2
        @ecm.the_code_pointer=("<pair><string>main</string><int>2</int></pair>")
        @ecm.the_code_pointer.should == {:main => 2}
      end

      it "should return the nil by default" do
        @ecm.the_code_pointer.should be_nil
      end
      it "should return nil even when given correct input if code has not been created" do
        @ecm.the_code_pointer=("<pair><string>main</string><int>2</int></pair>")
        @ecm.the_code_pointer.should be_nil
      end

      it "should return nil even when given correct input if the key is not in the code" do
        @ecm.the_code=CMAP_2
        @ecm.the_code_pointer=("<pair><string>junk</string><int>2</int></pair>")
        @ecm.the_code_pointer.should be_nil
      end
    end
  end
end