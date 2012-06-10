require 'exceptions/quantum_stack_model_invalid_create'


class SimulateResultsModel
  attr_accessor :simulate_results
  attr_accessor :simulate_results_text
  attr_accessor :random_value_text
  attr_accessor :stack_translation

  def random_value_text=(whatever)
  end

  def simulate_results_text=(whatever)
  end

  def simulate_results_text
    inner = (@simulate_results || []).collect {|triple| "#{triple[0]}(#{triple[1]}) = #{triple[2]}"}.join("<br />")
    "<html>"+inner+"</html>"
  end

  def random_value_text
    @random_value_text || ""
  end

  def simulate_results=(xml_data)
    raise QuantumStackModelInvalidCreate, "Missing Stack Translation" if @stack_translation.nil?
    sr = SIMULATE_PATTERN.match xml_data

    raise QuantumStackModelInvalidCreate, "Invalid Simulate Results: #{xml_data}" if !sr
    @random_value_text = "Random Value: "+sr[1]
    @simulate_results = SimulateResultsModel.result_values_to_list(sr[2],@stack_translation)
  end

  def self.result_values_to_list(rvals,stack_trans)
    return [] if !rvals or "" == rvals
    ret = []
    rv = TRIPLE_PATTERN.match(rvals)
    return ret if !rv
    matched_len = rv[0].length
    ret << [stack_trans.reverse_lookup(rv[1]),rv[2],rv[3]]
    while rv
      rv = TRIPLE_PATTERN.match(rvals[matched_len, rvals.length])
      return ret if !rv
      matched_len += rv[0].length
      ret << [stack_trans.reverse_lookup(rv[1]),rv[2],rv[3]]
    end
  end
  SIMULATE_PATTERN = Regexp.new /<Simulated><double>(.*)<\/double><results>((<triple><string>.*?<\/string><string>.*?<\/string><string>.*?<\/string><\/triple>)*)<\/results><\/Simulated>/

  TRIPLE_PATTERN = Regexp.new /<triple><string>(.*?)<\/string><string>(.*?)<\/string><string>(.*?)<\/string><\/triple>/
end
