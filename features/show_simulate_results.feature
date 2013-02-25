@load_coin
Feature: We can load a program, run it and simulate the results of observing

  As a researcher
  I want the program allow me to simulate the result

  Scenario:
    When I click the button "Go" 1 time on the frame "Quantum Emulator"
    Given I select "Simulate" from the "File" menu
    Then the dialog "Simulate Results" should have one of:
           |result|
           |a.*Heads|
           |a.*Tails|
