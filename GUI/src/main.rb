# encoding: utf-8
require 'config/manifest'
require 'utility/logger'
require 'config/pathing'
require 'swing_runner'
require 'lqpl_controller'

def _create_global_error_handler
  GlobalErrorHandler.on_error { |exception, thread| LqplUtility::Logger.new.log_the_error(exception, thread) }
end

def lqpl_main
  # SwingRunner.on_edt do
  LqplController.instance.open
# end
rescue => e
  LqplUtility::Logger.new.log_the_error(e)
end

Pathing.add_to_load_path('')
_create_global_error_handler
lqpl_main
