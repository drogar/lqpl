@compile
Feature: We can load and compile lqpl code

  As a researcher
  I want the program allow me to choose a "*.qpl" program and create a corresponding "*.qpo" file
  so I can start experimenting with LQPL

  Scenario: I compile a simple qpl program
    And I load "coin.qpl" from the project directory "GUI/testdata/qplprograms"
    Then "coin.qpo" should be created in the project directory "GUI/testdata/qplprograms" and be equal to "coin.reference.qpo"
    Then the messages field should contain:
      |partial|
      |Compile of coin.qpl was successful|

  Scenario: I try to compile a qpl program with syntax errors
    And I load "invalidsyntax.qpl" from the project directory "GUI/testdata/qplprograms"
    Then the messages field should contain:
      |partial|
      |invalidsyntax.qpl was unsuccessful|
      |unexpected|

  Scenario: I try to compile a qpl program with syntax errors
    And I load "invalidsemantics.qpl" from the project directory "GUI/testdata/qplprograms"
    Then the messages field should contain:
      |partial|
      |invalidsemantics.qpl was unsuccessful|
      |Semantic Error|

  Scenario: I compile a qpl program with warnings errors
    And I load "invalidbalance.qpl" from the project directory "GUI/testdata/qplprograms"
    Then the messages field should contain:
      |partial|
      |invalidbalance.qpl was successful|
      |Semantic Warning|
      |Unbalanced creation|
      |discarding d|
      |discarding c|