KVP1 = "<kvpair><key><string>p</string></key><value><int>1</int></value></kvpair>"
KVP2 = "<kvpair><key><string>p</string></key><value><int>2</int></value></kvpair>"
KVP3 = "<kvpair><key><string>p</string></key><value><int>3</int></value></kvpair>"
KVP1REX2 = KVP1 +
    "<kvpair><key><string>rex</string></key><value><int>2</int></value></kvpair>"

KVP27 = "<kvpair><key><string>p</string></key><value><int>27</int></value></kvpair>"
KVREX27 ="<kvpair><key><string>rex</string></key><value><int>27</int></value></kvpair>"
KVTH13 = "<kvpair><key><string>th</string></key><value><int>13</int></value></kvpair>"

P1 = "<MMap><map>"+KVP1+"</map></MMap>"
P2 = "<MMap><map>"+KVP2+"</map></MMap>"
P1ANDEMPTY = "<MMap><map></map><map>"+KVP1+"</map></MMap>"

EMPTYSTACK = "<MMap><map></map></MMap>"

Q1R2 = "<MMap><map><kvpair><key><string>@q</string></key><value><int>1</int></value></kvpair>"+
  "<kvpair><key><string>@r</string></key><value><int>2</int></value></kvpair></map></MMap>"


L3STACK = "<MMap><map>"+KVP1+"</map><map>"+KVP2+"</map><map>"+KVREX27+KVP3+"</map></MMap>"