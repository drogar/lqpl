require 'spec/spec_helper'

describe DuckMatcher do
  it "is created from a start and stop string, followed by a third string" do
    dm = DuckMatcher.new "start","stop", "entire"
    dm.should_not be_nil
  end
  describe "match" do
    before ("each") do
      @dm = DuckMatcher.new "strt","stop", "entire"
    end
    it "matches for the third element first and returns that if it exists" do
      m = @dm.match "entire"
      m.length.should == 1
      m[0].should == "entire"
    end
    it "returns nil if the input is empty" do
      m = @dm.match ""
      m.should be_nil
    end
    it "partitions the 'strtxxxstop' into a 1 element array ['strtxxxstop']" do
      m = @dm.match "strtxxxstop"
      m.length.should == 1
      m[0].should == "strtxxxstop"
    end
    it "partitions the 'strtxxxstopstrtxxxstop' into a 1 element array ['strtxxxstop']" do
      m = @dm.match "strtxxxstopstrtxxxstop"
      m.length.should == 1
      m[0].should == "strtxxxstop"
    end
    it "partitions the 'strtxxxstopstrtxxxstopstrtxxxstop' into a 1 element array ['strtxxxstop']" do
      m = @dm.match "strtxxxstopstrtxxxstopstrtxxxstop"
      m.length.should == 1
      m[0].should == "strtxxxstop"
    end
    it "partitions the 'strtxxstrtxxxstopxstop' into a 1 element array ['strtxxstrtxxxstopxstop']" do
      m = @dm.match "strtxxstrtxxxstopxstop"
      m.length.should == 1
      m[0].should == "strtxxstrtxxxstopxstop"
    end
    it "partitions the 'strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstopstrtxxxstop' into a 1 element array ['strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstop']" do
      m = @dm.match "strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstopstrtxxxstop"
      m.length.should == 1
      m[0].should == "strtxxstrtxstrtxxstrtxxxstopxstopxxstopxstop"
    end
  end
end      