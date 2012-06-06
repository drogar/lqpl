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
    md = LIST_PATTERN.match qubit_string
    raise InvalidInput, qubit_string if !md
    rval=[[self.translate_qubit(md[2]), self.translate_qubit(md[5])]]
    [1,2,3].each do |num_found|
      md = LIST_PATTERN.match(qubit_string[md[1].length*num_found,qubit_string.length])
      return rval if !md
      elem = [self.translate_qubit(md[2]), self.translate_qubit(md[5])]
      raise InvalidInput, "#{elem} duplicated in qubit" if rval.include? elem
      rval << elem
    end
    rval
  end

  def self.translate_qubit(single_qubit)
   case single_qubit
   when "<qz/>" then 0
   when "<qo/>" then 1
   else raise InvalidInput, "Got #{single_qubit}, expecting one of '<qz/>' or '<qo/>'"
   end
 end
end