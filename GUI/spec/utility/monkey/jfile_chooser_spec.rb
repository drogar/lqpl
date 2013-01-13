require 'spec/spec_helper'

describe JFileChooser do
  describe "class method opener" do
    before :each do
      @j = JFileChooser.opener("a","b","c")
    end
    it "should create a JFileChooser" do
      @j.should_not be_nil
    end
    it "should set the title to the first parameter" do
      @j.dialog_title.should == "a"
    end
    it "should set a file_filter whose description is the second parm" do
      @j.file_filter.description.should == "b"
    end
    it "should only accept files whose extension equals the third parm" do
      @j.file_filter.accept(java.io.File.new("~/junk.c")).should be_true
      @j.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
    end
  end
  describe "lqpl_assembled_file_opener" do
    before :each do
      @qpo = JFileChooser.lqpl_assembled_file_opener
    end
    it "should have a title of 'Load LQPO (Assembly) File'" do
      @qpo.dialog_title.should == "Load LQPO (Assembly) File"
    end
    it "should set a file_filter whose description is 'LQPL assembled file'" do
      @qpo.file_filter.description.should == "LQPL assembled file"
    end
    it "should only accept files whose extension is qpo" do
      @qpo.file_filter.accept(java.io.File.new("~/junk.qpo")).should be_true
      @qpo.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
    end
  end
  
  describe "lqpl_source_file_opener" do
    before :each do
      @qpl = JFileChooser.lqpl_source_file_opener
    end
    it "should have a title of 'Open LQPL File for Compiling'" do
      @qpl.dialog_title.should == "Open LQPL File for Compiling"
    end
    it "should set a file_filter whose description is 'LQPL source file'" do
      @qpl.file_filter.description.should == "LQPL source file"
    end
    it "should only accept files whose extension is qpo" do
      @qpl.file_filter.accept(java.io.File.new("~/junk.qpl")).should be_true
      @qpl.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
    end
  end
    
end
      
    