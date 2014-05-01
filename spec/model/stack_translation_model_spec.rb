require 'spec/spec_helper'

require 'spec/specdata/stack_translation_data'

require 'GUI/src/panels/stack_translation/stack_translation_model'

describe StackTranslationModel do
  it "should give an invalid create error when sent incorrect data" do
    expect {
      s = StackTranslationModel.new
      s.stack_translation= "err"
    }.to raise_error ParserError, /err/
  end
  it "should create a text representation in its text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    expect(s.text).to eq("<html><ol><li>p=>1</li></ol></html>")
    s.stack_translation= L3STACK
    expect(s.text).to eq("<html><ol><li>p=>1</li><li>p=>2</li><li>rex=>27, p=>3</li></ol></html>")
  end
  it "should ignore direct assignment to the text attribute" do
    s = StackTranslationModel.new
    s.stack_translation= P1
    s.text = "junk"
    expect(s.text).to eq("<html><ol><li>p=>1</li></ol></html>")
  end
  describe "it should provide reverse lookups" do
    before(:each) do
      @s = StackTranslationModel.new
    end
    it "should return the requested value if it is not found on reverse lookup" do
      expect(@s.reverse_lookup(15)).to eq("15")
    end
    it "should return the name when there is only one entry" do
      @s.stack_translation = P1
      expect(@s.reverse_lookup(1)).to eq("p")
    end
    it "should return the name of the first entry when there are repeated keys in multiple lists" do
      @s.stack_translation = P1ANDR1
      expect(@s.reverse_lookup(1)).to eq("p")
      @s.stack_translation = P1ANDR1ANDS1
      expect(@s.reverse_lookup(1)).to eq("p")
      @s.stack_translation = P1ANDEMPTYANDS1
      expect(@s.reverse_lookup(1)).to eq("p")
    end
    it "should return the keys for all the values in the list of maps" do
      @s.stack_translation = L3STACK

      expect(@s.reverse_lookup(1)).to eq("p")
      expect(@s.reverse_lookup(2)).to eq("p")
      expect(@s.reverse_lookup(3)).to eq("p")
      expect(@s.reverse_lookup(27)).to eq("rex")
    end
  end
end

