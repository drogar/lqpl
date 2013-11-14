require 'spec/spec_helper'
describe TranslateLineEnds do
  it "changes a line end to <\\\\n>" do
    "#{TranslateLineEnds.new "\n"}".should == "<\\n>"
  end
  it "makes no changes to a string without \\n" do
    "#{TranslateLineEnds.new 'abc'}".should == "abc"
  end
  it "changes all the \\n's in an input string to <\\\\n>" do
    "#{TranslateLineEnds.new "abc\n123\nwho boy\n"}".should == "abc<\\n>123<\\n>who boy<\\n>"
  end
  it "changes all the line endings in the file min.qpo" do
    File.open("#{TEST_QP_PATH}/min.reference.qpo", "r") do |f|
      qpl_file_data = f.read()
      "#{TranslateLineEnds.new qpl_file_data}".should == "Compiler: Version=0.9.1<\\n>app_fcdlbl0   Start<\\n>EnScope<\\n>DeScope<\\n>    Return 0<\\n>   EndProc<\\n>"
    end
  end

end
