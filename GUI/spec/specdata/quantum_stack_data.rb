def make_multi_sstacks(front,back,sstack,count)
  front+"<substacks>"+(sstack*count)+"</substacks>" + back
end

QSQBZero = "<Qstack><int>1</int><bool>True</bool>"+
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>1.0</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
QSVAL5 = "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"

QSZ = "<Qstack><int>1</int><bool>True</bool><substacks></substacks><Zero/></Qstack>"
QSQBHAD = make_multi_sstacks("<Qstack><int>1</int><bool>True</bool>",
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack>",
  QSVAL5,4)

QSINT = make_multi_sstacks("<Qstack><int>3</int><bool>True</bool>",
  "<Classical><cint>27</cint><cint>5</cint></Classical></Qstack>",
  QSVAL5,2)


QSQ1R2="<Qstack><int>2</int><bool>True</bool>"+ #r
  "<substacks><Qstack><int>1</int><bool>True</bool>"+ #q
  "<substacks><Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>False</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack>"+
  "<Qstack><int>-1</int><bool>True</bool><substacks></substacks><Value><number>0.5</number></Value></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair><pair><qz/><qo/></pair><pair><qo/><qz/></pair><pair><qo/><qo/></pair></Qubits></Qstack></substacks>"+
  "<Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"

QB2WITHBOTTOM =  "<Qstack><int>2</int><bool>True</bool><substacks><bottom/></substacks><Qubits><pair><qz/><qz/></pair></Qubits></Qstack>"
C1WITHBOTTOM =  "<Qstack><int>2</int><bool>True</bool><substacks><bottom/></substacks><Classical><cint>27</cint></Classical></Qstack>"
AL1WITHBOTTOM =  "<Qstack><int>7</int><bool>False</bool><substacks><bottom/></substacks><AlgebraicData><string>Nil</string><StackAddresses></StackAddresses></AlgebraicData></Qstack>"

BOTTOMS = [QB2WITHBOTTOM,C1WITHBOTTOM,AL1WITHBOTTOM]

MIXED=[QSQBZero, QB2WITHBOTTOM,QSVAL5]

ADDRESSES=[[1,QSQBZero],[-1,QSVAL5], [2,QB2WITHBOTTOM]]

DIAGS=[[true,QSQBZero], [true,QSVAL5], [false,AL1WITHBOTTOM]]

QS3LEVEL = make_multi_sstacks("<Qstack><int>3</int><bool>True</bool>",
  "<Classical><cint>27</cint><cint>5</cint><cint>7</cint></Classical></Qstack>",
  QSQBZero,3)