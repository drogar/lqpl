def make_multi_sstacks(id, diag, back, sstacks)
  '{"qstack" : {"id": ' + id + ', "diagonal" : ' + diag + ', "substacks": [' +
    sstacks.join(',') + '],' + back + '}}'
end

def bottom_stack(id, diag, kind, count = 1)
  '{"qstack" : {"id": ' + id + ', "diagonal" : ' + diag +
    ', "substacks": [' + (['{"bottom":true}'] * count).join(', ') + '],' +
    kind + '}}'
end

NODE_ZERO = '"qnode" : {"value": 1.0}'.freeze
NODE_VAL1 = '"qnode" : {"value": 1.0}'.freeze
NODE_VAL5 = '"qnode" : {"value": 0.5}'.freeze
NODE_QZZ = '"qnode" : {"qubit": ["ZZ"]}'.freeze
NODE_QALL = '"qnode" : {"qubit": ["ZZ","ZO", "OZ", "ZZ"]}'.freeze
NODE_CL275 = '"qnode" : {"classical":[27,5]}'.freeze
NODE_CL2757 = '"qnode" : {"classical":[27,5,7]}'.freeze
NODE_DATA = '"qnode" : {"data": [{"cons": "Nil", "addresses" : []}]}'.freeze

QSVAL1 = make_multi_sstacks('-1', 'true', NODE_ZERO, [])
QSVAL5 = make_multi_sstacks('-1', 'true', NODE_VAL5, [])
QSVAL5F = make_multi_sstacks('-1', 'false', NODE_VAL5, [])

QSQBZero = make_multi_sstacks('1', 'true', NODE_QZZ, [QSVAL1])

QSZ = make_multi_sstacks('1', 'true', NODE_ZERO, [])

QSQBHAD = make_multi_sstacks('1', 'true', NODE_QZZ,  [QSVAL5] * 4)

QSINT = make_multi_sstacks('3', 'true', NODE_CL275,  [QSVAL5] * 2)

QSQ1R2 = make_multi_sstacks('2', 'true', NODE_QZZ, # first r, then q
                            [make_multi_sstacks('1', 'true', NODE_QALL,
                                                [QSVAL5, QSVAL5F, QSVAL5F, QSVAL5])])

QB2WITHBOTTOM = bottom_stack('2', 'true', NODE_QZZ)
C1WITHBOTTOM = bottom_stack('2', 'true', NODE_CL275)
AL1WITHBOTTOM = bottom_stack('7', 'false', NODE_DATA)

BOTTOMS = [QB2WITHBOTTOM, C1WITHBOTTOM, AL1WITHBOTTOM].freeze

MIXED = [QSQBZero, QB2WITHBOTTOM, QSVAL5].freeze

ADDRESSES = [[1, QSQBZero], [-1, QSVAL5], [2, QB2WITHBOTTOM]].freeze

DIAGS = [[true, QSQBZero], [true, QSVAL5], [false, AL1WITHBOTTOM]].freeze

QS3LEVEL = make_multi_sstacks('3', 'true', NODE_CL2757, [QSQBZero] * 3)
