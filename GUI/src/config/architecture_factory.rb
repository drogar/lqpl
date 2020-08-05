require 'architecture_mac'
require 'architecture_linux'
require 'architecture_win'

class ArchitectureFactory
  def self.architecture_category(config_map = RbConfig::CONFIG)
    case config_map['host_os']
    when /darwin/i
      ArchitectureMac.new
    when /linux/i
      ArchitectureLinux.new
    when /^win|mswin/i
      ArchitectureWin.new
    end
  end
end
