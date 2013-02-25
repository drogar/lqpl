class SwingRunner
  def initialize(explicit_block=nil, &block)
    @block = explicit_block || block
  end
  def run
    @block.call
  end


  # :nocov:
  def self.on_edt(&task)
    if javax.swing.SwingUtilities.event_dispatch_thread?
      javax.swing.SwingUtilities.invoke_later SwingRunner.new(task)
    else
      javax.swing.SwingUtilities.invoke_and_wait SwingRunner.new(task)
    end
  end
  # :nocov:

end