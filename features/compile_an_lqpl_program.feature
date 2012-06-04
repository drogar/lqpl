Feature: We can load and assemble lqpl code

  As a researcher
  I want the program allow me to choose a "*.qpl" program and create a corresponding "*.qpo" file
  so I can start experimenting with LQPL


  Scenario: I load and assemble a simple qpl program
    Given I select "Compile" from the "File" menu
    And I load "coin.qpl" from the directory "testdata/qplprograms"
    Then "coin.qpo" should be created in "testdata/qplprograms" and be equal to "coin.reference.qpo"
