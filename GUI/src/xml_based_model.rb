class XmlBasedModel <ApplicationModel
  include XmlDecode
  attr_accessor :value
  attr_accessor :name


  def check_and_return_value(pattern,in_string,data_parser)
    matched = pattern.match in_string
    raise ModelCreateError, "Invalid input for #{self.class}: #{in_string}" if ! matched
    data_parser.call matched[1]
  end

end