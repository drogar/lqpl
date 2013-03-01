require 'spec/spec_helper'


describe AboutModel do
  subject {AboutModel.new}
  specify {subject.about_text.should == ABOUT_STRING}
end