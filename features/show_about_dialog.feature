Feature: We can see a dialog with the version and a link to the website
  As a researcher
  I want the program to identify itself

  Scenario:
    Given I select "About" from the "Help" menu
    Then the dialog "About LQPL" should have one of the following in its only label:
           |result|
           |Brett Giles|
           |Robin|
