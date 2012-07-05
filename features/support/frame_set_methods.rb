module JemmyFrameSetup
  def frame_name_var_string(frame_name)
    case frame_name
    when "Quantum Emulator" then "$qe_frame"
    else "@#{frame_name.downcase.gsub(/ /,'_')}"
    end
  end

  def set_frame_name_var(frame_name)
    case frame_name
    when "Quantum Emulator" then return
    else fnvs = frame_name_var_string(frame_name)
      eval "#{fnvs} = JFrameOperator.new frame_name"
    end
  end
end

World(JemmyFrameSetup)