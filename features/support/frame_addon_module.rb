module FrameAddon
  def frame_ref_var_string(frame_title)
    case frame_title
    when "Quantum Emulator" then "$qe_frame"
    else "@#{frame_title.downcase.gsub(/ /,'_')}"
    end
  end

  def set_frame_ref_var(frame_title)
    case frame_title
    when "Quantum Emulator" then return
    else 
      # todo - Need to get all components, loop through them and check against
      # a FrameMatcher to get the right frame, then create. 
      # use $robot to get ComponentHierarchy - get roots, check the roots, then
      # check the children recursively and so forth.
      
      fnvs = frame_ref_var_string(frame_title)
      eval "#{fnvs} = WindowFinder.find_frame(FrameMatcher.with_title (\"#{frame_title}\")).using($robot)"
    end
  end
  
end

World(FrameAddon)