require 'spec/spec_helper'

describe JFileChooser do
  describe "class method opener" do
    before :each do
      SwingRunner::on_edt do
        @j = JFileChooser.opener("a","b","c")
      end
    end
    it "should create a JFileChooser" do
      @j.should_not be_nil
    end
    it "should set the title to the first parameter" do
      SwingRunner::on_edt do
        @j.dialog_title.should == "a"
      end
    end
    it "should set a file_filter whose description is the second parm" do
      SwingRunner::on_edt do
        @j.file_filter.description.should == "b"
      end
    end
    it "should only accept files whose extension equals the third parm" do
      SwingRunner::on_edt do
        @j.file_filter.accept(java.io.File.new("~/junk.c")).should be_true
        @j.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
      end
    end
  end
  describe "lqpl_assembled_file_opener" do
    before :each do
      SwingRunner::on_edt do
        @qpo = JFileChooser.lqpl_assembled_file_opener
      end
    end
    it "should have a title of 'Load LQPO (Assembly) File'" do
      SwingRunner::on_edt do
        @qpo.dialog_title.should == "Load LQPO (Assembly) File"
      end
    end
    it "should set a file_filter whose description is 'LQPL assembled file'" do
      SwingRunner::on_edt do
        @qpo.file_filter.description.should == "LQPL assembled file"
      end
    end
    it "should only accept files whose extension is qpo" do
      SwingRunner::on_edt do
        @qpo.file_filter.accept(java.io.File.new("~/junk.qpo")).should be_true
        @qpo.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
      end
    end
  end
  
  describe "lqpl_source_file_opener" do
    before :each do
      SwingRunner::on_edt do
        @qpl = JFileChooser.lqpl_source_file_opener
      end
    end
    it "should have a title of 'Open LQPL File for Compiling'" do
      SwingRunner::on_edt do
        @qpl.dialog_title.should == "Open LQPL File for Compiling"
      end
    end
    it "should set a file_filter whose description is 'LQPL source file'" do
      SwingRunner::on_edt do
        @qpl.file_filter.description.should == "LQPL source file"
      end
    end
    it "should only accept files whose extension is qpo" do
      SwingRunner::on_edt do
        @qpl.file_filter.accept(java.io.File.new("~/junk.qpl")).should be_true
        @qpl.file_filter.accept(java.io.File.new("~/junk.jun")).should be_false
      end
    end
  end
    
end
      
    