require 'exceptions/quantum_stack_model_invalid_create'

class StackTranslationModel < XmlBasedModel
  attr_accessor :stack_translation
  attr_accessor :text

  def stack_translation=(in_mmap)
    @stack_translation = StackTranslationModel::decode_mmap(in_mmap)
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

  def self.decode_mmap(in_mmap)
    return [] if in_mmap == ""
    ret = []
    match_list_of_maps = MMAP_PATTERN.match in_mmap
    raise QuantumStackModelInvalidCreate, in_mmap if !match_list_of_maps
    lom = match_list_of_maps[1]
    list_elem = LIST_ELEMENT_PATTERN.match(lom)
    return [] if !list_elem
    list_elems_len = list_elem[0].length
    while list_elem
      ret << StackTranslationModel::kv_pairs_to_map(list_elem[1])
      list_elem = LIST_ELEMENT_PATTERN.match(lom[list_elems_len, lom.length])
      return ret if !list_elem
      list_elems_len += list_elem[0].length
    end
  end

  def self.kv_pairs_to_map(kvps)
    return {} if !kvps or kvps == ""
    ret = {}
    kvp = KVPATTERN.match kvps
    return ret if !kvp
    ret[kvp[1].to_sym] = kvp[2].to_i
    matched_len = kvp[0].length
    while kvp
      kvp = KVPATTERN.match(kvps[matched_len,kvps.length])
      return ret if !kvp
      ret[kvp[1].to_sym] = kvp[2].to_i
      matched_len += kvp[0].length
    end
  end

  MMAP_PATTERN = Regexp.new /^<MMap>(.*)<\/MMap>$/
  LIST_ELEMENT_PATTERN = Regexp.new /^<map>(.*?)<\/map>/
  KVPATTERN = Regexp.new /^<kvpair><key><string>(.*?)<\/string><\/key><value><int>(\d*)<\/int><\/value><\/kvpair>/

end
