module RaceConditions
  def sleep_until(tries, &condition)
    count = 0
    while count < tries
      sleep 0.25
      return true if condition.call
      count += 1
    end
    return false
  end
  
  def sleep_until_not(tries, &condition)
    sleep_until(tries) {!condition.call}
  end
  
  def sleep_until_file_exists(tries,file_name)    
    sleep_until(tries) {File.exist?(file_name)}
  end
  
  def sleep_until_visibility(tries,component,condition)
    sleep_until(tries) {component.edt_visible? == condition}
  end
end

World(RaceConditions)