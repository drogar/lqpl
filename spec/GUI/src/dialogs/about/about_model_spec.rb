require 'about_model'

describe AboutModel do
  subject { AboutModel.new }
  specify { expect(subject.about_text).to eq(ABOUT_STRING) }
end
