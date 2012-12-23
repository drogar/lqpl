# 
# Before do
# 
# end
# 
# After do
# end

# TODO - reconsider trying this (Around) again if there is a way to cleanly stop / start the 
# system while testing.
#
# java_import java.awt.event.InputEvent
# java_import java.awt.event.KeyEvent

# Around do |sc, blk|
# 
#   runner = GuiActionRunner.execute(AppStarter.new)
#   $robot = BasicRobot.robot_with_current_awt_hierarchy
#   $qe_frame = FrameFixture.new($robot, "Quantum Emulator")
#   blk.call
#   $robot.press_modifiers(InputEvent::META_MASK)
#   $robot.press_key(KeyEvent::VK_Q)
#   $robot.release_key(KeyEvent::VK_Q)
# #  $qe_frame.close
#   
#   $robot.clean_up
#   $robot = nil
# 
#   $qe_frame = nil
#   #LqplController.instance.close
#   runner = nil
#   sleep 2
# end
