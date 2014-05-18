# encoding: utf-8
#  model for the tree to do the quantum stack
class QuantumStackModel < ApplicationModel
  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :stack_translation
  attr_accessor :stackaddress
  attr_accessor :bottom
  alias_method :bottom?, :bottom

  def quantum_stack
  end

  def quantum_stack=(in_qstack)
    fail ModelCreateError, 'QuantumStack: Missing Stack Translation' if @stack_translation.nil?
    return unless in_qstack
    @preferred_size = nil
    decode_stack_data in_qstack
  end

  def decode_stack_data(in_qstack)
    qpp = EnsureJSON.new(in_qstack).as_json
    @bottom = qpp.key?(:bottom)
    @substacks = []
    return if bottom?
    the_stack = qpp[:qstack]
    @stackaddress = the_stack[:id]
    @on_diagonal = the_stack[:diagonal]
    @substacks = make_substacks(the_stack)
    self.descriptor = the_stack[:qnode]
  end

  def make_substacks(qs_json)
    return nil unless qs_json[:substacks]
    qs_json[:substacks].map do |ss|
      q = QuantumStackModel.new
      q.stack_translation = @stack_translation
      q.quantum_stack = ss
      q
    end
  end

  def descriptor=(json_descriptor)
    @descriptor = AbstractDescriptorModel.make_instance json_descriptor
    @descriptor.class.validate_substacks_count(@substacks)
    @descriptor.name = make_name(:use_stack_address)
  end

  def make_name(formatting)
    nm = (@stack_translation.reverse_lookup(@stackaddress)).to_s
    nm += "(#{@stackaddress})" if nm != "#{@stackaddress}" && formatting == :use_stack_address
    nm = '' if nm == '-1'
    nm
  end
end
