module XmlDecode
  module ClassMethods
    def values_to_list(input,pattern)
      return [] if !input or "" == input
      return_list = []
      match_vals = pattern.match input
      return return_list if ! match_vals
      matched_len = match_vals[0].length
      yield return_list,match_vals
      while match_vals
        match_vals = pattern.match input[matched_len, input.length]
        return return_list if ! match_vals
        matched_len += match_vals[0].length
        yield return_list,match_vals
      end
    end
  end

  def self.included(klass)
   klass.extend(ClassMethods)
 end

end