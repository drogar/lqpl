class QfaceView < ApplicationView
  set_java_class com.drogar.qface.screens.QuantumEmulatorMainScreen
  map :view => "controlPanel.visible", :model => :control_panel_visible
  map :view => "stepSpinner.model.value", :model => :step_spinner
  map :view => "stepButton.enabled", :model => :step_enabled
  map :view => "goButton.enabled", :model => :go_enabled

end
