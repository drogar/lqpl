KVPS6='<i>EnScope</i><i>QLoad "@q" 0</i><i>QApply 0 Hadamard "@q"</i>'+
  '<i>QPullup "@q"</i><i>EnScope</i><i>Measure "@q" 14 6 10</i>'

KVPAIRS_2 = '<kvpair><key><string>main</string></key>'+
        '<value><instructions><i>EnScope</i></instructions></value></kvpair>'+
        '<kvpair><key><string>cflip_fcdelbl0</string></key>'+
        '<value><instructions><i>EnScope</i><i>QLoad "@q" 0</i></instructions></value></kvpair>'

CMAP_SINGLE = '<Code><map><kvpair><key><string>main</string></key>'+
        '<value><instructions><i>EnScope</i></instructions></value></kvpair>'+
        '</map></Code>'
CMAP_6 = '<Code><map><kvpair><key><string>main</string></key>'+
        '<value><instructions>'+KVPS6+'</instructions></value></kvpair>'+
        '</map></Code>'
CMAP_2x6 = '<Code><map><kvpair><key><string>main</string></key>'+
        '<value><instructions>'+KVPS6+KVPS6+'</instructions></value></kvpair>'+
        '</map></Code>'
CMAP_2='<Code><map>'+KVPAIRS_2+'</map></Code>'

RES_CMAP_2 = {:main => ["  0  EnScope"], :cflip_fcdelbl0 => ["  0  EnScope", '  1  QLoad "@q" 0']}