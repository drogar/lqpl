# encoding: utf-8
# Model for the simulate results dialog
class SimulateResultsModel
  attr_accessor :simulate_results
  attr_accessor :simulate_results_text
  attr_accessor :random_value_text
  attr_accessor :stack_translation

  java_signature 'void random_value_text(Object)'
  def random_value_text=(whatever)
  end

  java_signature 'void simulate_results_text(Object)'
  def simulate_results_text=(whatever)
  end

  def simulate_results_text
    inner = (@simulate_results || []).map { |res| "#{res[0]}(#{res[1]}) = #{res[2]}" }
    '<html>' + inner.join('<br />') + '</html>'
  end

  def random_value_text
    @random_value_text || ''
  end

  java_signature 'void simulate_results(Object)'
  def simulate_results=(xml_data)
    fail ModelCreateError, 'Missing Stack Translation' if @stack_translation.nil?
    sr = SimulateResultsParser.new xml_data
    @random_value_text = 'Random Value: ' + sr.random_value
    @simulate_results = sr.simulate_results.map do |srs|
      [@stack_translation.reverse_lookup(srs[0]), srs[1], srs[2]]
    end
  end
end
