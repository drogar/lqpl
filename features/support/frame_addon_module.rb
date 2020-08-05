# module to assist with swing frames
module FrameAddon
  def frame_ref_var_string(frame_title)
    return '$qe_frame' if frame_title == 'Quantum Emulator'

    "@#{frame_title.downcase.tr(' ', '_')}"
  end

  def frame_fixture(frame_title)
    return $qe_frame if frame_title == 'Quantum Emulator'

    fnvs = frame_ref_var_string(frame_title)
    eval "#{fnvs} = WindowFinder.find_frame(FrameMatcher.with_title (\"#{frame_title}\")).using($robot)", binding, __FILE__, __LINE__
    eval fnvs, binding, __FILE__, __LINE__
  end
end

World(FrameAddon)
