class Array
  def partion_into_left_mid_right
    [get_left_partition , _get_middle_element_as_array, get_right_partition]
  end

  def get_partition(side)
    return get_left_partition if side == :left
    return get_right_partition if side == :right
    _get_middle_element_as_array
  end

  def get_left_partition
    self[0,self.size/2]
  end

  def get_right_partition
    self[(-(self.size/2)).ceil,self.size/2]
  end

  def get_middle_element
    _get_middle_element_as_array[0]
  end

  def tails
    Range.new(0,(self.size-1)).collect do |i|
      self[i,self.size-i]
    end
  end

  def heads
    Range.new(0,(self.size-1)).collect do |i|
      self[0,i+1]
    end
  end

  private
  def _get_middle_element_as_array
    half_size = self.size/2
    self[half_size,-((-self.size/2).ceil+(half_size.floor))]
  end

end