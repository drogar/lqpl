def make_multi_sstacks(front,back,sstack,count)
  front+"<substacks>"+(sstack*count)+"</substacks>" + back
end

QSQBZero = "<Qstack><int>1</int><bool>True</bool>"+
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
QSVAL5 = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"

QSQBHAD = make_multi_sstacks("<Qstack><int>1</int><bool>True</bool>",
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack>",
  QSVAL5,4)

KVP1 = "<kvpair><key><string>p</string></key><value><int>1</int></value></kvpair>"
KVP1REX2 = KVP1 +
    "<kvpair><key><string>rex</string></key><value><int>2</int></value></kvpair>"

KVP27 = "<kvpair><key><string>p</string></key><value><int>27</int></value></kvpair>"
KVREX27 ="<kvpair><key><string>rex</string></key><value><int>27</int></value></kvpair>"
KVTH13 = "<kvpair><key><string>th</string></key><value><int>13</int></value></kvpair>"

P1 = "<MMap><map>"+KVP1+"</map></MMap>"
P1ANDEMPTY = "<MMap><map></map><map>"+KVP1+"</map></MMap>"

Q1R2 = "<MMap><map><kvpair><key><string>@q</string></key><value><int>1</int></value></kvpair>"+
  "<kvpair><key><string>@r</string></key><value><int>2</int></value></kvpair></map></MMap>"

QSQ1R2="<Qstack><int>2</int><bool>True</bool>"+ #r
  "<substacks><Qstack><int>1</int><bool>True</bool>"+ #q
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"