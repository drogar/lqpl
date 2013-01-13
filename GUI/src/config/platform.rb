#===============================================================================
# Platform specific operations, feel free to remove or override any of these
# that don't work for your platform/application

def on_mac
 yield if RbConfig::CONFIG["host_os"] =~ /darwin/i
end

#:nocov:
def on_win
  yield if  RbConfig::CONFIG["host_os"] =~ /^win|mswin/i 
end

def on_linux
  yield if  RbConfig::CONFIG["host_os"] =~ /^win|mswin/i 
end


def not_on_mac
  yield if  !(RbConfig::CONFIG["host_os"] =~ /darwin/i)
end
#:nocov: 
on_mac do
  testing = java.lang.System.get_property("com.drogar.testing.fest")
  if !testing or testing != "true"
    java.lang.System.set_property("apple.laf.useScreenMenuBar", "true")
  end
end
