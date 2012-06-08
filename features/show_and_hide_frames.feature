Feature: We can load and run an assembled file at the server

  As a researcher
  I want the program allow me to choose a "*.qpo" program and load it to the server so I can execute it.

  Background:
    When I select "Load" from the "File" menu
    And I load "coin.reference.qpo" from the directory "testdata/qplprograms"

  Scenario:
    Then the frame "Executing Code" should be visible
    When I select "Hide Executing Code" from the "View" menu
    Then the frame "Executing Code" should not be visible
    When I select "Show Executing Code" from the "View" menu
    Then the frame "Executing Code" should be visible

