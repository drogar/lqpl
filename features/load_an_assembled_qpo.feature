Feature: We can load and run an assembled file at the server

  As a researcher
  I want the program allow me to choose a "*.qpo" program and load it to the server so I can execute it.

  Background:
    When I select "Load" from the "File" menu
    And I load "coin.reference.qpo" from the directory "testdata/qplprograms"


  Scenario:
    Then the frame "Executing Code" should be visible
    When I click the button "Step" 1 time on the frame "Quantum Emulator"
    Then the selection on the frame "Executing Code" should show ---  1  Call 0 "cflip_fcdlbl0"

  Scenario:
    Then the button "Step" should appear
    Then the button "Go" should appear
    Then the button "Trim" should appear
    Then the number spinner "Step Size" with value "1" should appear
    Then the number spinner "Recursion Depth" with value "10" should appear
    Then the number spinner "Tree Depth" with value "4" should appear
    Then the frame "Quantum Stack" should be visible

