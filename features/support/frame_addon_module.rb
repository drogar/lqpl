# encoding: UTF-8
# module to assist with swing frames
module FrameAddon
  def frame_ref_var_string(frame_title)
    return  '$qe_frame' if frame_title == 'Quantum Emulator'

    "@#{frame_title.downcase.gsub(/ /, '_')}"
  end

  def set_and_return_frame_fixture(frame_title)
    return $qe_frame if frame_title == 'Quantum Emulator'

    fnvs = frame_ref_var_string(frame_title)
    eval "#{fnvs} = WindowFinder.find_frame(FrameMatcher"\
         ".with_title (\"#{frame_title}\")).using($robot)"
    eval fnvs
  end
end

World(FrameAddon)
