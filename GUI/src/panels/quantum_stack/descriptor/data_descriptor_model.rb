# encoding: utf-8
# Date node model
class DataDescriptorModel < AbstractDescriptorModel
  def self.validate_substacks_count(substacks)
    fail ModelCreateError,
         'Data element on stack should have substacks' if !substacks || substacks.size == 0
  end

  def self.data_value_valid(dv)
    cons_ok(dv) && address_ok(dv) && address_elements_ok(dv)
  end

  def self.cons_ok(dv)
    dv && dv[:cons]  && dv[:cons].kind_of?(String)
  end

  def self.address_ok(dv)
    dv[:addresses] && dv[:addresses].kind_of?(Array)
  end

  def self.address_elements_ok(dv)
    dv[:addresses].each do |a|
      return false unless a && a.kind_of?(Integer)
    end
  end

  def initialize(in_string)
    fail_message = "Invalid Algebraic data: #{in_string}"
    json_d = EnsureJSON.new(in_string).as_json
    @value = json_d[:data]
    fail ModelCreateError, fail_message unless @value && @value.kind_of?(Array)
    @value.each do |dv|
      fail ModelCreateError, fail_message unless DataDescriptorModel.data_value_valid(dv)
    end
  end

  def length
    @value.length
  end

  def substack_labels
    @value.map { |v| "#{v[:cons]}#{v[:addresses] if [] != v[:addresses]}" }
  end
end
