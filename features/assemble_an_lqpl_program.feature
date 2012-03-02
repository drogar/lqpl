Feature: We can load and assemble lqpl code

  As a researcher
  I want the program allow me to choose a "*.qpl" program and create a corresponding "*.qpo" file
  so I can start experimenting with LQPL
Background:
  Given the program "com.drogar.qface.Main" has started
  Given the frame "Quantum Emulator" is visible


  Scenario: I load and assemble a simple qpl program
    Given I select "Assemble" from the file menu
    And I load "coinflip.qpl" from the directory "./qplprograms"
    Then "coinflip.qpo" should be created in "./qpofiles"
    