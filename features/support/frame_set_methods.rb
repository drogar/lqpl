module FrameSetup
  def frame_name_var_string(frame_name)
    case frame_name
    when "Quantum Emulator" then "$qe_frame"
    else "@#{frame_name.downcase.gsub(/ /,'_')}"
    end
  end

  def set_frame_name_var(frame_name)
    case frame_name
    when "Quantum Emulator" then return
    else 
      # todo - Need to get all components, loop through them and check against
      # a FrameMatcher to get the right frame, then create. 
      # use $robot to get ComponentHierarchy - get roots, check the roots, then
      # check the children recursively and so forth.
      
      fnvs = frame_name_var_string(frame_name)
      eval "#{fnvs} = FrameFixture.new($robot,FrameMatcher.with_title \"#{frame_name}\")"
    end
  end
end

World(FrameSetup)