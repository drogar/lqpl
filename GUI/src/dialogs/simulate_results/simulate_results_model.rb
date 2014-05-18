# encoding: utf-8
# Model for the simulate results dialog
class SimulateResultsModel
  attr_accessor :simulate_results
  attr_accessor :simulate_results_text
  attr_accessor :random_value_text
  attr_accessor :stack_translation

  def random_value_text=(_)
  end

  def simulate_results_text=(_)
  end

  def simulate_results_text
    inner = (@simulate_results || []).map { |name, type, cons| "#{name}(#{type}) = #{cons}" }
    '<html>' + inner.join('<br />') + '</html>'
  end

  def random_value_text
    @random_value_text || ''
  end

  def simulate_results=(sim_data)
    fail ModelCreateError, 'Missing Stack Translation' if @stack_translation.nil?
    sr = EnsureJSON.new(sim_data).as_json
    @random_value_text = "Random Value: #{sr[:Simulated]}"
    @simulate_results = sr[:results].map do |address, type, cons|
      [@stack_translation.reverse_lookup(address), type, cons]
    end
  end
end
