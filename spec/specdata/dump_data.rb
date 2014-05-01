require 'spec/specdata/classical_stack_data'

DCALL="<DumpCall><int>5</int><string>Ret</string><Classical></Classical></DumpCall>"
DCALL1="<DumpCall><int>7</int><string>some_method</string><Classical><cint>5</cint></Classical></DumpCall>"
DCALL2="<DumpCall><int>7</int><string>some_method</string><Classical><cint>5</cint><cbool>True</cbool></Classical></DumpCall>"

DSPLIT= "<DumpSplit>whatever</DumpSplit>"

DUMPSINGLECALL = "<Dump>" +DCALL+"</Dump>"

DUMPTWOCALL = "<Dump>" +DCALL+DCALL2+"</Dump>"
DUMPCALLANDSPLIT = "<Dump>" +DCALL+DSPLIT+"</Dump>"
DUMPCALLSPLITCALL = "<Dump>" +DCALL+DSPLIT+DCALL2+"</Dump>"
DUMPSPLIT = "<Dump>" +DSPLIT+"</Dump>"