# class to make sure swing components execute only on the  swing thread
class SwingRunner
  SWING_UTILS = javax.swing.SwingUtilities

  def initialize(explicit_block = nil, &block)
    @block = explicit_block || block
  end

  def run
    @block.call
  end

  def self.on_edt(&task)
    if SWING_UTILS.event_dispatch_thread?
      SWING_UTILS.invoke_later SwingRunner.new(task)
    else
      SWING_UTILS.invoke_and_wait SwingRunner.new(task)
    end
  end
end
