class StackTranslationModel < ApplicationModel
  attr_accessor :stack_translation
  attr_accessor :text

  def stack_translation=(in_mmap)
    stp = StackTranslationParser.new in_mmap
    @stack_translation = stp.parsed_value
    @reverse_translation = @stack_translation.reverse.inject({}) do |rev_map,st_map|
      rev_map.merge! st_map.invert
    end
  end

  def text=(whatever)
  end

  def text
    inside = @stack_translation.inject("") do |inner, tr_map|
      line = (tr_map.collect {|kv| "#{kv[0]}=>#{kv[1]}"}).join(", ")
      inner += "<li>"+line+"</li>"
    end
    "<html><ol>"+inside+"</ol></html>"
  end

  def reverse_lookup(val)
    rmap = @reverse_translation || {}
    rmap.default=val.to_s.to_sym
    rmap[val.to_i].id2name
  end


end
