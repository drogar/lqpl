require 'spec/spec_helper'

describe Array do
  describe "get_left_partition" do
    it "should get first 2 of a size four array" do
      [1,2,3,4].get_left_partition.should == [1,2]
    end    
    it "should get first 2 of a size five array into 2,1,2" do
      [1,2,3,4,5].get_left_partition.should == [1,2]
    end
    it "should get first 1 of a size two array" do
      [1,2].get_left_partition.should == [1]
    end
    it "should get first 1 of a size three array" do
      [1,2,3].get_left_partition.should == [1]
    end
    it "should get empty array from a size one array" do
      [1].get_left_partition.should == []
    end
    it "should get empty array from  a size zero array" do
      [].get_left_partition.should == []
    end
  end
  
  describe "get middle element" do
    it "should get nil for a size 4 array" do
      [1,2,3,4].get_middle_element.should be_nil
    end    
    it "should get the 3rd element of a size five array" do
      [1,2,3,4,5].get_middle_element.should == 3
    end
    it "should get nil for  a size two array into 1,0,1" do
      [1,2].get_middle_element.should be_nil
    end
    it "should get the second element of a size three array" do
      [1,2,3].get_middle_element.should == 2
    end
    it "should get the only element of a size one array" do
      [1].get_middle_element.should == 1
    end
    it "should get nil for  a size zero array" do
      [].get_middle_element.should be_nil
    end
  end
  
  describe "get_right_partition" do
    it "should get the last 2 of a size four array" do
      [1,2,3,4].get_right_partition.should == [3,4]
    end    
    it "should get the last 2 of a size five array" do
      [1,2,3,4,5].get_right_partition.should == [4,5]
    end
    it "should get the last  of a size two array" do
      [1,2].get_right_partition.should == [2]
    end
    it "should get the last  of a size three array" do
      [1,2,3].get_right_partition.should == [3]
    end
    it "should get empty from a size one array" do
      [1].get_right_partition.should == []
    end
    it "should get empty from a size zero array" do
      [].get_right_partition.should == []
    end
  end
  
  describe "partition into left mid right" do
    it "should split a size four array into 2,0,2" do
      [1,2,3,4].partion_into_left_mid_right.should == [[1,2],[],[3,4]]
    end    
    it "should split a size five array into 2,1,2" do
      [1,2,3,4,5].partion_into_left_mid_right.should == [[1,2],[3],[4,5]]
    end
    it "should split a size two array into 1,0,1" do
      [1,2].partion_into_left_mid_right.should == [[1],[],[2]]
    end
    it "should split a size three array into 1,1,1" do
      [1,2,3].partion_into_left_mid_right.should == [[1],[2],[3]]
    end
    it "should split a size one array into 0,1,0" do
      [1].partion_into_left_mid_right.should == [[],[1],[]]
    end
    it "should split a size zero array into 0,0,0" do
      [].partion_into_left_mid_right.should == [[],[],[]]
    end
  end
  describe "tails" do
    it "gives [] for []" do
      [].tails.should == []
    end
    
    it "gives [[1]] for [1]" do
      [1].tails.should == [[1]]
    end
    
    it "gives [[1,2],[2]] for [1,2]" do
      [1,2].tails.should == [[1,2],[2]]
    end
    
    it "gives [[1,2,3],[2,3],[3]] for [1,2,3]" do
      [1,2,3].tails.should ==  [[1,2,3],[2,3],[3]]
    end
  end
  
  
  describe "heads" do
    it "gives [] for []" do
      [].heads.should == []
    end
    
    it "gives [[1]] for [1]" do
      [1].heads.should == [[1]]
    end
    
    it "gives [[1],[1,2]] for [1,2]" do
      [1,2].heads.should == [[1],[1,2]]
    end
    
    it "gives [[1],[1,2],[1,2,3]] for [1,2,3]" do
      [1,2,3].heads.should ==  [[1],[1,2],[1,2,3]]
    end
  end
end