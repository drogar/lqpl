One of the goals of this project is to have a full suite of automated tests.

In the JRuby portion of this, we use FEST for testing of the swing components, Cucumber
to run "acceptance" tests and rspec to run unit tests. In each of those, we use ruby's
'simplecov' gem to determine test coverage. However, there are a few notes regarding these
combinations:

 - Cucumber and FEST is the only way to test Swing related code. FEST testing just does not
   seem to work in RSPEC. Note that for Cucumber coverage to be complete, we must call
   "LqplController.instance.open" to start the app, rather than using the java main. This
   is due to the java main starting JRuby in a new process (without simplecov).

 - With rspec, FEST and simplecov, the coverage analysis is complete, but FEST will not drive
   any items that are opened. Hence, we can not add unit tests for methods such as the opening
   of the file dialogs in lqpl_controller.

 - Some items are only run on Windows or Linux computers - testing is intended to be run on
   a Mac, therefore these are surrounded with :nocov: tags to remove them from the stats.

Ignoring nocov tags, the coverage goal for the project GUI will be 97.5%, with no file being less
than 95%. These are not added as part of the configuration of simplecov as this is an additive
requirement. Either rspec or cucumber may be below this minimum provided the total is above.


In the Haskell portions, the system was completely written prior to embracing a test driven
development strategy. Test will be retrofitted as time permits or as code changes are made.
Hspec is used in testing the Haskell code. We intend to add in hpc for coverage in the future.