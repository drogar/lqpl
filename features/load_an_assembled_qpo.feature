Feature: We can load and run an assembled file at the server

  As a researcher
  I want the program allow me to choose a "*.qpo" program and load it to the server so I can execute it.

  Background:
    When I select "Load" from the "File" menu
    And I load "coin.reference.qpo" from the directory "testdata/qplprograms"

  @wip
  Scenario:
    Then the main frame's title should be "Quantum Emulator - coin.reference.qpo"
    Then the button "Step" should appear
    Then the button "Go" should appear
    Then the button "Trim" should appear
    Then the number spinner "Step Size" should appear and have value "1"
    Then the number spinner "Recursion Depth" should appear and have value "1"
    Then the number spinner "Recursion Multiplier" should appear and have value "10"
    Then the number spinner "Tree Depth" should appear and have value "4"
    Then the frame "Quantum Stack" should be visible

  Scenario:
    Then the frame "Executing Code" should be visible
    When I click the button "Step" 1 time on the frame "Quantum Emulator"
    Then the selection on the frame "Executing Code" should show ---  1  Call 0 "cflip_fcdlbl0"
    When I click the button "Step" 3 times on the frame "Quantum Emulator"
    Then the selection on the frame "Executing Code" should show ---  2  QApply 0 Hadamard "@q"
    Then I click the spinner "Step Size" up 2 times on the frame "Quantum Emulator"
    When I click the button "Step" 1 time on the frame "Quantum Emulator"
    Then the selection on the frame "Executing Code" should show ---  5  Measure "@q" 14 6 10

  Scenario:
    Then the frame "Executing Code" should be visible
    When I click the button "Go" 1 time on the frame "Quantum Emulator"
    Then the selection on the frame "Executing Code" should show ---  3  DeScope
    Then the button "Go" on the frame "Quantum Emulator" should be disabled
    Then the button "Step" on the frame "Quantum Emulator" should be disabled

  Scenario:
    Then the frame "Classical Stack" should be visible
    Then the frame "Dump" should be visible
    Then the frame "Stack Translation" should be visible




