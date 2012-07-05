require 'spec/spec_helper'

require 'src/panels/executable_code/executable_code_view'
require 'src/panels/executable_code/code_pointer'


CP = CodePointer.new("<pair><string>main</string><int>17</int></pair>")

describe ExecutableCodeView do
  describe 'class method make_selection_key' do
    it "should combine the qpo method and a line number to make a key" do
      ExecutableCodeView::make_selection_key(:a,1).should == "a--1"
    end
  end
  describe 'class method mangle_code_pointer_to_selection_key' do
    it "should combine the code pointer method and line number" do
      ExecutableCodeView::mangle_code_pointer_to_selection_key(CP).should == "main--17"
    end
  end
end