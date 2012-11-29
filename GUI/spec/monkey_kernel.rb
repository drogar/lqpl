# This file is available in: https://raw.github.com/japgolly/golly-utils

module Kernel

  # Alternate implementation of `at_exit` that preserves the exit status (unless you call `exit` yourself and an error
  # is raised).
  #
  # The initial driver for this was that using `at_exit` to clean up global resources in RSpec tests, RSpec's exit
  # status would be lost which means CI processes and such were unable to tell whether there were test failures.
  #
  # @return [Proc] Whatever `at_exit` returns.
  def at_exit_preserving_exit_status(&block)
    at_exit {
      status= $!.is_a?(::SystemExit) ? $!.status : nil
      block.()
      exit status if status
    }
  end
end
