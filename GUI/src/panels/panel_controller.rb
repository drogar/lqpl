class PanelController < ApplicationController
  
  @@controllers={}
  def self.controller_from_name(name_array)
    name_array.collect(&:to_sym).inject(nil){|memo, n| memo || @@controllers[n]}
  end

  def update_on_lqpl_model_trim
    false
  end
  
end


class DumpController < PanelController
  @@controllers[:Dump] = DumpController
end

class ClassicalStackController < PanelController
  @@controllers[:Classical] = ClassicalStackController
end

class QuantumStackController < PanelController
  @@controllers[:Quantum] = QuantumStackController
end

class ExecutableCodeController < PanelController
  @@controllers[:Executing] = ExecutableCodeController
end

class StackTranslationController < PanelController
  @@controllers[:Translation] = StackTranslationController
end
  