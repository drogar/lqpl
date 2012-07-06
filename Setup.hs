import Distribution.Simple
-- Trying new format, but it doesn't work, therefore reverting.
main=defaultMain

--import Distribution.PackageDescription(PackageDescription)
--import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
--import System.Cmd(system)
--import Distribution.Simple.LocalBuildInfo


--main = defaultMainWithHooks (simpleUserHooks {runTests = runTheTests})

--runTheTests:: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
--runTheTests a b pd lb = system ( "cd src ; runghc Tests/RunTests.lhs") >> return()