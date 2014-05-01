\begin{code}
  module Utility.TestExtras where
    import Test.HUnit

    import Utility.Extras


    tests =  ["t1 " ~: "String with all printable not changed" ~: "abc" @=? (filterNonPrintable "abc"),
      "t2 " ~: "String with \\n loses that" ~: "ab" @=? (filterNonPrintable "ab\n"),
      "t3 " ~: assertBool "String with \\n not retained"  ("a\n" /= (filterNonPrintable "a\n")),
      "noext " ~: "Base + empty = '',base,''" ~: ("./","abc","") @=? splitFilePath "abc",
      "splitFilePath1 " ~: "Base + ext = ./,base,ext" ~: ("./","abc","123") @=? splitFilePath "abc.123",
      "splitFilePath2 " ~: "rpath1/Base + ext = rpath1,base,ext" ~: ("rpath/","abc","123") @=? splitFilePath "rpath/abc.123",
      "splitFilePath3 " ~: "apath1/Base + ext = apath1,base,ext" ~: ("/apath/","abc","123") @=? splitFilePath "/apath/abc.123",
      "splitFilePath2 " ~: "rpathn/Base + ext = rpathn,base,ext" ~: ("r/p/ath/","abc","123") @=? splitFilePath "r/p/ath/abc.123",
      "splitFilePath3 " ~: "apathn/Base + ext = apathm,base,ext" ~: ("/a/pa/th/","abc","123") @=? splitFilePath "/a/pa/th/abc.123"]

\end{code}