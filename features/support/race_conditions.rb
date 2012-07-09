module RaceConditions
  def sleep_until(tries, &condition)
    count = 0
    while count < tries
      sleep 0.25
      break if condition.call
      count += 1
    end
  end
end

World(RaceConditions)