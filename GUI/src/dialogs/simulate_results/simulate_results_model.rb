
class SimulateResultsModel
  include XmlDecode
  attr_accessor :simulate_results
  attr_accessor :simulate_results_text
  attr_accessor :random_value_text
  attr_accessor :stack_translation

  java_signature "void random_value_text(Object)"
  def random_value_text=(whatever)
  end

  java_signature "void simulate_results_text(Object)"
  def simulate_results_text=(whatever)
  end

  def simulate_results_text
    inner = (@simulate_results || []).collect {|triple| "#{triple[0]}(#{triple[1]}) = #{triple[2]}"}.join("<br />")
    "<html>"+inner+"</html>"
  end

  def random_value_text
    @random_value_text || ""
  end

  java_signature "void simulate_results(Object)"
  def simulate_results=(xml_data)
    raise ModelCreateError, "Missing Stack Translation" if @stack_translation.nil?
    sr = SIMULATE_PATTERN.match xml_data

    raise ModelCreateError, "Invalid Simulate Results: #{xml_data}" if !sr
    @random_value_text = "Random Value: "+sr[1]
    @simulate_results = SimulateResultsModel.result_values_to_list(sr[2],@stack_translation)
  end

  def self.result_values_to_list(rvals,stack_trans)
    values_to_list rvals, TRIPLE_PATTERN  do |ret, rv|
      ret << [stack_trans.reverse_lookup(rv[1]),rv[2],rv[3]]
    end
  end
  SIMULATE_PATTERN = Regexp.new /<Simulated><double>(.*)<\/double><results>((<triple><string>.*?<\/string><string>.*?<\/string><string>.*?<\/string><\/triple>)*)<\/results><\/Simulated>/

  TRIPLE_PATTERN = Regexp.new /<triple><string>(.*?)<\/string><string>(.*?)<\/string><string>(.*?)<\/string><\/triple>/
end
