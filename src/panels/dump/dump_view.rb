
class DumpView < ApplicationView
  set_java_class com.drogar.qface.screens.Dump
  map   :view => "dump.text", :model => :text
end
