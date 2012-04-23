Feature: We can load and run an assembled file at the server

  As a researcher
  I want the program allow me to choose a "*.qpo" program and load it to the server so i can execute it.
Background:
  Given the program "com.drogar.qface.Main" has started
  Given the frame "Quantum Emulator" is visible


  Scenario:
    When I select "Load" from the "File" menu
    And I load "coin.reference.qpo" from the directory "testdata/qplprograms"
    Then the frame "Emulator Control" should appear

  Scenario:
    Given I have just loaded "coin.reference.qpo" from the directory "testdata/qplprograms"
    When I click the button "Step"
    Then the "Machine State" field should show "<qpl>"