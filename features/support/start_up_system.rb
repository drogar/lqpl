# class to actually start the system
class AppStarter < GuiQuery
  # Launch the app in the Event Dispatch Thread (EDT),
  # which is the thread reserved for user interfaces.
  # FEST will call this method for us before the test.
  #
  # Must be named as is - alias does not work - this is a Java class call
  def executeInEDT
    LqplController.instance.open # com.drogar.lqpl.Main.main([])
  end
end

# module to perform specific actions of the system
module StartUpSystem
  def start_up_lqpl
    GuiActionRunner.execute(AppStarter.new)
    $robot = BasicRobot.robot_with_current_awt_hierarchy
    $qe_frame = FrameFixture.new($robot, 'Quantum Emulator')
  end

  def click_menu_item(menu_hierarchy)
    menu_item = $qe_frame.menu_item_with_path menu_hierarchy.to_java(:string)
    menu_item.click
    sleep 0.25 # needed otherwise things may close before the next check....
  end

  def approve_file(dir, file)
    fc = JFileChooserFixture.new($robot) #   $qe_frame.file_chooser()
    fc.select_file_in_project_directory(dir, file)
    fc.approve
  end
end

World(StartUpSystem)
