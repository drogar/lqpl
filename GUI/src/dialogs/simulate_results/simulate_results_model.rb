require 'ensure_json'
# Model for the simulate results dialog
class SimulateResultsModel
  attr_reader :simulate_results
  attr_accessor :stack_translation

  def simulate_results_text=(_unused); end

  def simulate_results_text
    inner = (@simulate_results || []).map { |sr| "#{sr[0]}(#{sr[1]}) = #{sr[2]}" }
    '<html>' + inner.join('<br />') + '</html>'
  end

  def random_value_text
    @random_value_text || ''
  end

  def simulate_results=(sim_data)
    raise ModelCreateError, 'Missing Stack Translation' if @stack_translation.nil?

    sim_results = EnsureJSON.new(sim_data).as_json
    self.random_value_text = sim_results
    @simulate_results = map_sim_results(sim_results)
  end

  def random_value_text=(sim_results)
    @random_value_text = "Random Value: #{sim_results[:Simulated]}"
  rescue StandardError
    @random_value_text = ''
  end

  private

  def map_sim_results(sim_results)
    sim_results[:results].map do |result|
      [@stack_translation.reverse_lookup(result[0].to_i), result[1], result[2]]
    end
  end
end
