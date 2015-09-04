# encoding: utf-8
require 'stack_translation_form'

# ST view, get the text out.
class StackTranslationView < ApplicationView
  set_java_class StackTranslationForm
  map view: 'stack_translation_text', model: :text
end
