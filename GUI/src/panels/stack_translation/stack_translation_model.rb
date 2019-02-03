# model for the stack translation display
class StackTranslationModel < ApplicationModel
  attr_reader :stack_translation

  def stack_translation=(in_mmap)
    json_stp = EnsureJSON.new(in_mmap).as_json

    @stack_translation = json_stp[:memory_map]
    @reverse_translation = @stack_translation.reverse.reduce({}) do |rev_map, st_map|
      rev_map.merge! st_map.invert
    end
  end

  def text=(_unused); end

  def text
    inside = @stack_translation.reduce('') do |inner, tr_map|
      line = (tr_map.map { |kv| "#{kv[0]}=>#{kv[1]}" }).join(', ')
      inner + '<li>' + line + '</li>'
    end
    '<html><ol>' + inside + '</ol></html>'
  end

  def reverse_lookup(val)
    rmap = @reverse_translation || {}
    rmap.default = val.to_s.to_sym
    rmap[val.to_i].id2name
  end
end
