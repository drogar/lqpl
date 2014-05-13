module Fixture.CompilerData  where

    program_one = "{ \"file_name\" : \"f\", \"qpl_program\" : [ \"qdata C = {H|T}\", \"app::(| ; )= {skip}\"] }"
    program_two = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"#Import f\"] }"
    program_three = "{ \"file_name\" : \"h\", \"qpl_program\" : [ \"#Import g\"] }"
    program_bad = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"qdata Coin = {head}\"] }"
    program_bad2 = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"main::() = { q = |0>; purejunk q}\"] }"
    program_bad3 = "{ \"file_name\" : \"g\", \"qpl_program\" : [ \"main::() = { q = |0>; measure q of |0> => {c=1} |1> => {d=2}}\"] }"

    assembled_one = "{\"qpo\" : [\"app_fcdlbl0   Start\""

    jsonSendVersion = "{\"command\" : \"send_version\"}"

    getfile_command = "{\"send_file\" : \"f\"}"

