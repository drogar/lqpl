@startit
Feature: we can cancel the compile and open dialogs
  As we may have selected the wrong dialog, we need to be able to cancel them.

  Scenario: I cancel compiling a program
    Given I select "Compile" from the "File" menu
    And I cancel the dialog
    Then the messages field should contain:
      |partial|
      |Compile action cancelled|


  Scenario: I cancel loading a program
    Given I select "Load" from the "File" menu
    And I cancel the dialog
    Then the messages field should contain:
      |partial|
      |QPO file load cancelled|

