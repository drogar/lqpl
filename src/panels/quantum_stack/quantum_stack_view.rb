
class QuantumStackView < ApplicationView
  set_java_class com.drogar.qface.screens.QuantumStackFrame
  map  :view => "quantumStackPanel.quantumStack", :model => :qstack
end
