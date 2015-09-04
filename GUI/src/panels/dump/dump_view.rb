# encoding: utf-8
require 'dump_form'

# View for the Dump
class DumpView < ApplicationView
  set_java_class DumpForm
  map view: 'dump_text', model: :text
end
