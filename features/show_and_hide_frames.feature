Feature: We can show and hide the various frames

  As a researcher
  I want the program allow me to show only what i want.

	Background:
	  When I select "Load" from the "File" menu
    And I load "coin.reference.qpo" from the project directory "GUI/testdata/qplprograms"

  Scenario:
    Then the frame "Executing Code" should be visible
    When I select "Hide Executing Code" from the "View" menu
    Then the frame "Executing Code" should not be visible
    When I select "Show Executing Code" from the "View" menu
    Then the frame "Executing Code" should be visible


  Scenario:
    Then the frame "Stack Translation" should be visible
    When I select "Hide Stack Translation" from the "View" menu
    Then the frame "Stack Translation" should not be visible
    When I select "Show Stack Translation" from the "View" menu
    Then the frame "Stack Translation" should be visible


  Scenario:
    Then the frame "Dump" should be visible
    When I select "Hide Dump" from the "View" menu
    Then the frame "Dump" should not be visible
    When I select "Show Dump" from the "View" menu
    Then the frame "Dump" should be visible


  Scenario:
    Then the frame "Classical Stack" should be visible
    When I select "Hide Classical Stack" from the "View" menu
    Then the frame "Classical Stack" should not be visible
    When I select "Show Classical Stack" from the "View" menu
    Then the frame "Classical Stack" should be visible