require 'specdata/quantum_stack_data'
require 'specdata/stack_translation_data'

DCALL = '{"dump_call": {"return_label": 3, "return_ep" : "Ret", "classical": {"cstack" : []}}}'.freeze

DCALL1 = '{"dump_call": {"return_label": 7, "return_ep" : "some_method", '\
         '"classical": {"cstack" : [5]}}}'.freeze
DCALL2 = '{"dump_call": {"return_label": 7, "return_ep" : "some_method", '\
         '"classical": {"cstack" : [5, true]}}}'.freeze

DSPLIT = '{"dump_split" : {"return_label": 3, "branches" : [{"qsbranch": ' + QSVAL1 +
         ', "branch_label":1}], "qsresult": ' + QSQBZero +
         ', "save_cstack" : {"cstack" : [5]}, "save_ns": {"int_list": [1,2], "address" : 15}' \
         ', "result_ns":  {"int_list": [5,6], "address" : 22}' \
         ', "save_stacktrans" :' + P1 +
         ', "result_stacktrans": ' + P2 + '}}'

DUMPSINGLECALL = '{"dump" : [' + DCALL + ']}'

DUMPTWOCALL = '{"dump" : [' + DCALL + ', ' + DCALL2 + ']}'
DUMPCALLANDSPLIT = '{"dump" : [' + DCALL + ', ' + DSPLIT + ']}'
DUMPCALLSPLITCALL = '{"dump" : [' + DCALL + ', ' + DSPLIT + ', ' + DCALL2 + ']}'
DUMPSPLIT = '{"dump" : [' + DSPLIT + ']}'
