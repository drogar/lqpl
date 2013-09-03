
class SimulateResultsModel
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
    sr = SimulateResultsParser.new xml_data
    @random_value_text = "Random Value: "+sr.random_value
    @simulate_results = sr.simulate_results.collect {|srs| [@stack_translation.reverse_lookup(srs[0]),srs[1],srs[2]]}
  end

end
