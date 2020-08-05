KVPS6 = '"EnScope", "QLoad \"@q\" 0", "QApply 0 Hadamard \"@q\"","QPullup \"@q\"",'\
        '"EnScope","Measure \"@q\" 14 6 10"'.freeze

KVPAIRS_2 = '{"main": ["EnScope"], "cflip_fcdelbl0": ["EnScope","QLoad \"@q\" 0"]}'.freeze

CMAP_SINGLE = '{"main": ["EnScope"]}'.freeze

CMAP_6 = '{"main": [' + KVPS6 + ']}'

CMAP_2x6 = '{"main": [' + KVPS6 + ', ' + KVPS6 + ']}'

CMAP_2 = KVPAIRS_2
