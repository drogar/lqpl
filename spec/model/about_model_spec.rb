# Encoding : UTF-8
require 'spec/spec_helper'

describe AboutModel do
  subject { AboutModel.new }
  specify { expect(subject.about_text).to eq(ABOUT_STRING) }
end
