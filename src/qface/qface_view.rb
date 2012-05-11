class QfaceView < ApplicationView
  set_java_class com.drogar.qface.screens.QuantumEmulatorMainScreen
  map :view => "controlPanel.visible", :model => :control_panel_visible

end
