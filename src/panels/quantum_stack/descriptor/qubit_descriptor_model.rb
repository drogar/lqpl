class QubitDescriptorModel < AbstractDescriptorModel

  PATTERN=Regexp.new /^<Qubits>((<pair>((<qz\/>)|(<qo\/>))((<qz\/>)|(<qo\/>))<\/pair>){1,4})<\/Qubits>$/

  LIST_PATTERN = Regexp.new /^(<pair>((<qz\/>)|(<qo\/>))((<qz\/>)|(<qo\/>))<\/pair>)/
  # match 2 and 5

  def initialize(in_string)
    matc = PATTERN.match in_string
    raise StackDescriptorModelInvalidCreate, in_string if !matc
    @value = QubitDescriptorModel::parse_list matc[1]
  end

  def length
    @value.length
  end

  def substack_labels
    @value.collect {|v| "#{v[0]}#{v[1]}"}
  end


  def self.parse_list(qubit_string)
    raise InvalidInput, "Must have qubit indicators" if !qubit_string or qubit_string.length == 0
    r = values_to_list qubit_string, LIST_PATTERN, do |rval,md|
      elem = [self.translate_qubit(md[2]), self.translate_qubit(md[5])]
      raise InvalidInput, "#{elem} duplicated in qubit" if rval.include? elem
      rval << elem
    end

    raise InvalidInput, qubit_string if r.length > 4

    raise InvalidInput, qubit_string if r.length == 0 and qubit_string and qubit_string.length > 0
    r
  end

  def self.translate_qubit(single_qubit)
   case single_qubit
   when "<qz/>" then 0
   when "<qo/>" then 1
   else raise InvalidInput, "Got #{single_qubit}, expecting one of '<qz/>' or '<qo/>'"
   end
 end
end