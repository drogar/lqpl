class ServerConnection
  private_class_method :new

  def ServerConnection.instance
    @@instance = new unless @@instance
    @@instance
  end

  def new
  end

  def send_load_from_file_name(filename)
  end
end
