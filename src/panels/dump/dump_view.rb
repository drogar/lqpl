
class DumpView < ApplicationView
  set_java_class com.drogar.lqpl.screens.Dump
  map   :view => "dump.text", :model => :text
end
