
class ApplicationController < Monkeybars::Controller
  # Add content here that you want to be available to all the controllers
  # in your application

  attr_accessor :lqpl_emulator_server_connection
  @@controllers={}

  java_signature "void lqpl_emulator_server_connection(Object)"
  def lqpl_emulator_server_connection=(sc)
    @lqpl_emulator_server_connection = sc
    @lqpl_emulator_server_connection.connect if !@lqpl_emulator_server_connection.connected?
  end

  # doing a suspenders and belt here - will assume default SC if there isn't one.
  java_signature "Object lqpl_emulator_server_connection()"
  def lqpl_emulator_server_connection
    @lqpl_emulator_server_connection = LqplEmulatorServerConnection.instance if !@lqpl_emulator_server_connection
    @lqpl_emulator_server_connection
  end

  def my_frame
    @__view.the_frame
  end
  
  def toggle_visibility
    visible? ? hide : show
  end
  
  def self.controller_from_name(name_array)
    name_array.collect(&:to_sym).inject(nil){|memo, n| memo || @@controllers[n]}
  end

end

class DumpController < ApplicationController
  @@controllers[:Dump] = DumpController
end

class ClassicalStackController < ApplicationController
  @@controllers[:Classical] = ClassicalStackController
end

class QuantumStackController < ApplicationController
  @@controllers[:Quantum] = QuantumStackController
end

class ExecutableCodeController < ApplicationController
  @@controllers[:Executing] = ExecutableCodeController
end

class StackTranslationController < ApplicationController
  @@controllers[:Translation] = StackTranslationController
end
  