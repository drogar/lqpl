P1 =  '{"memory_map" :[{"p" : 1}]}'.freeze
P2 =  '{"memory_map" :[{"r" : 1}]}'.freeze
P1WITHREX27 = '{"memory_map" :[{"p" : 1, "rex": 27}]}'.freeze
P1ANDR1 = '{"memory_map" :[{"p" : 1}, {"r": 1}]}'.freeze
P1ANDR1ANDS1 = '{"memory_map" :[{"p" : 1}, {"r": 1}, {"s": 1}]}'.freeze

P1ANDEMPTYANDS1 = '{"memory_map" :[{"p" : 1}, {}, {"s": 1}]}'.freeze
P1ANDEMPTY = '{"memory_map" :[{}, {"p" : 1}]}'.freeze

EMPTYSTACK = '{"memory_map" :[{}]}'.freeze

Q1R2 = '{"memory_map" :[{"@q":1, "@r" :2}]}'.freeze

L3STACK = '{"memory_map" :[{"p" : 1},{"p" : 2},{"rex": 27, "p":3}]}'.freeze
