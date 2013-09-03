@load_recurse

Feature: We can change the call depth and multiplier
  The call depth controls how many calls the emulator will do before specifying the 
  result as undefined. The deeper the call depth, the better the approximation of
  the end result. However, the deeper the call depth, the more compute power used by the
  emulator.
  Additionally, the emulator can be set to use a multiplier for each approximation. For
  example, it can allow 20 calls per level. This is much cheaper computationally than 
  just displaying the 20th level where each one is a single call allowed.
  The GUI allows complete control over both these settings


  Scenario: I can set the multiplier to 1 by typing in the field
    Given I type "1" in the "Recursion Multiplier" field
    Then the messages field should contain:
      |partial|
      |Recursion Multiplier set|

  Scenario: I can set the recursion depth to 4 by typing in the field
    Given I type "4" in the "Recursion Depth" field
    Then the messages field should contain:
      |partial|
      |Recursion Depth set|

  Scenario: I can set the tree depth to 10 by typing in the field
    Given I type "10" in the "Tree Depth" field
    Then the messages field should contain:
      |partial|
      |Tree Depth set|

  Scenario: I can set the multiplier to 4 by typing in the field and then clicking
    Given I type "1" in the "Recursion Multiplier" field
    And I click the up on the "Recursion Multiplier" spinner 3 times
    Then the messages field should contain:
      |partial|
      |Recursion Multiplier set to 4|

  Scenario: I can set the recursion depth to 4 by typing in the field and then clicking
    Given I type "1" in the "Recursion Depth" field
    And I click the up on the "Recursion Depth" spinner 3 times
    Then the messages field should contain:
      |partial|
      |Recursion Depth set to 4|

  Scenario: I can set the tree depth to 5 by typing in the field and then clicking
    Given I type "10" in the "Tree Depth" field
    And I click the down on the "Tree Depth" spinner 5 times
    Then the messages field should contain:
      |partial|
      |Tree Depth set to 5|