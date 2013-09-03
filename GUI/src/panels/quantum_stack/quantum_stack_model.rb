class QuantumStackModel < ApplicationModel

  attr_accessor :substacks
  attr_accessor :descriptor
  attr_accessor :stack_translation
  attr_accessor :stackaddress

  def quantum_stack
  end

  def quantum_stack=(in_qstack)
    raise ModelCreateError, "QuantumStack: Missing Stack Translation" if @stack_translation.nil?
    return if !in_qstack
    @preferred_size = nil
    decode_stack_data_from_xml in_qstack
  end
  
  def decode_stack_data_from_xml(in_qstack)
    qpp = QuantumStackParser.new  in_qstack
    @bottom = qpp.bottom?
    @substacks = []
    return if bottom?
      
    @stackaddress = qpp.stackaddress
    @on_diagonal = qpp.on_diagonal?
    @substacks = qpp.substacks.collect do |ss| 
      q = QuantumStackModel.new
      q.stack_translation = @stack_translation
      q.quantum_stack = ss
      q
    end  if qpp.substacks
    set_descriptor qpp.descriptor
  end
  
  def set_descriptor(xml_descriptor)
    @descriptor = AbstractDescriptorModel.make_instance xml_descriptor
    @descriptor.class.validate_substacks_count(@substacks)
    @descriptor.name = make_name(:use_stack_address)
  end
  
  def make_name(formatting)
    nm = (@stack_translation.reverse_lookup(@stackaddress)).to_s
    nm += "(#{@stackaddress})" if nm != "#{@stackaddress}" && formatting == :use_stack_address
    nm = "" if nm == "-1"
    nm
  end

  def bottom?
    @bottom
  end


end
