require 'spec/spec_helper'
require 'src/panels/executable_code/executable_code_model'
require 'spec/specdata/executable_code_data'

describe ExecutableCodeModel do
  describe "public instance methods" do
    describe "the_code" do
      before(:each) do
        @ecm = ExecutableCodeModel.new
      end
      it "should throw an exception with bad input" do
        expect {@ecm.the_code="junk"}.to raise_error ParserError, /junk/
      end
      it "should make a singleton list when there is one <i> and </i> pair and prepend '  0  ' to the item" do
        @ecm.the_code = CMAP_SINGLE
        @ecm.the_code[:main].should == ["  0  EnScope"]
      end
      it "should make a list of all items between <i> and </i> pairs and prepend the index of the item " do
        @ecm.the_code = CMAP_6
        @ecm.the_code[:main].should == ['  0  EnScope',
          '  1  QLoad "@q" 0',
          '  2  QApply 0 Hadamard "@q"',
          '  3  QPullup "@q"',
          '  4  EnScope',
          '  5  Measure "@q" 14 6 10']
      end
      it "should properly justify indexes when the index > 0 " do
        @ecm.the_code = CMAP_2x6
        @ecm.the_code[:main][9,11].should == [
          '  9  QPullup "@q"',
          ' 10  EnScope',
          ' 11  Measure "@q" 14 6 10']
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
        expect {@ecm.the_code_pointer="junk"}.to raise_error ParserError, /junk/
      end
      it "should return the created code map when given correct input when there is code" do
        @ecm.the_code=CMAP_2
        @ecm.the_code_pointer=("<pair><string>main</string><int>0</int></pair>")
        @ecm.the_code_pointer.qpo_method.should == :main
        @ecm.the_code_pointer.line_number.should == 0
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
      it "should restrict the range of the pointer to the actual number of lines of code" do
        @ecm.the_code=CMAP_2
        @ecm.the_code_pointer=("<pair><string>main</string><int>17</int></pair>")
        @ecm.the_code_pointer.line_number.should == 0

        @ecm.the_code_pointer=("<pair><string>cflip_fcdelbl0</string><int>0</int></pair>")
        @ecm.the_code_pointer.line_number.should == 0

        @ecm.the_code_pointer=("<pair><string>cflip_fcdelbl0</string><int>1</int></pair>")
        @ecm.the_code_pointer.line_number.should == 1

        @ecm.the_code_pointer=("<pair><string>cflip_fcdelbl0</string><int>2</int></pair>")
        @ecm.the_code_pointer.line_number.should == 1

        @ecm.the_code_pointer=("<pair><string>cflip_fcdelbl0</string><int>3</int></pair>")
        @ecm.the_code_pointer.line_number.should == 1
      end
    end
  end
end

describe CodePointer do
  describe "creation" do
    it "should throw an exception with bad input" do
      expect {CodePointer.new("junk")}.to raise_error ParserError, /junk/
    end
    it "should throw an exception with nil input" do
      expect {CodePointer.new(nil)}.to raise_error ParserError, /No match/
    end
    it "should create a bare pointer with input ''" do
      cp=CodePointer.new("")
      cp.qpo_method.should == ""
      cp.line_number.should == 0
    end
    it "should create a CodePointer instance with correct input " do
      @cp = CodePointer.new("<pair><string>main</string><int>0</int></pair>")
      @cp.qpo_method.should == :main
      @cp.line_number.should == 0
    end
  end

  describe "instance method normalize" do
    before(:each) do
      @cp = CodePointer.new("<pair><string>main</string><int>17</int></pair>")
    end
    it "should change the line_number to zero when input 1" do
      @cp.normalize(1)
      @cp.line_number.should == 0
    end
    it "should change the line_number to 5 when input 6" do
      @cp.normalize(6)
      @cp.line_number.should == 5
    end
    it "should leave the line_number at 17 when input 18 or more" do
      @cp.normalize(18)
      @cp.line_number.should == 17
      @cp.normalize(24)
      @cp.line_number.should == 17
    end

    it "should change the line_number to 0 when input a negative number" do
      @cp.normalize(-5)
      @cp.line_number.should == 0
    end
    it "should change the line_number to 0 when input nil" do
      @cp.normalize(nil)
      @cp.line_number.should == 0
    end
    it "should change the line_number to 0 when input zero" do
      @cp.normalize(0)
      @cp.line_number.should == 0
    end
  end
  describe "mangle_to_selection_key" do
    it "should return method--linenumber" do
      @cp = CodePointer.new("<pair><string>main</string><int>17</int></pair>")
      @cp.mangle_to_selection_key.should == "main--17"
    end
  end
end
