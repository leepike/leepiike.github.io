#!/bin/csh
#
# Proof commands for reintegrate.sal
#
# Usage: reint <trace_file>
#
# WRITTEN BY BRUNO DUTERTRE (adapted by Lee Pike)


if ($# == 1) then 
   set tracefile=$1
else
   echo "Usage: $0 <trace_file>"
   exit 0
endif

set prfcmd="sal-inf-bmc --disable-expensive-ba-opt --disable-traceability -i"
set file="reint.sal"


set context=reint.sal

# proofs: <lemmas> <goal>

set proofs
set proofs=($proofs -d 1 mode_cntrl)
set proofs=($proofs -d 1 frame_prop)
set proofs=($proofs -d 1 pd_finish)
set proofs=($proofs -d 1 pd_init_op_accs)
set proofs=($proofs -l pd_finish -l mode_cntrl -d 4 op_seen_less2)
set proofs=($proofs -l mode_cntrl -l pd_init_op_accs -d 3 op_seen_more1)
set proofs=($proofs -l op_seen_more1 -l op_seen_less2 -d 1 pd_no_op_accs)
set proofs=($proofs -d 1 pd_not_fs_seen)
set proofs=($proofs -d 1 pd_not_sc_seen)
set proofs=($proofs -l pd_no_op_accs -d 1 fs_init_no_op_accs)
set proofs=($proofs -l pd_no_op_accs -l fs_init_no_op_accs -l frame_prop -l pd_not_fs_seen -d 3 fs_frame_gap)
set proofs=($proofs -l mode_cntrl -l pd_not_fs_seen -l fs_init_no_op_accs -l pd_no_op_accs -l frame_prop -d 3 fs_window)
set proofs=($proofs -l mode_cntrl -l pd_not_fs_seen -l fs_init_no_op_accs -l pd_no_op_accs -l frame_prop -l fs_window -d 3 fs_no_op_accs)
set proofs=($proofs -l pd_not_sc_seen -d 1 fs_not_sc_seen)
set proofs=($proofs -l fs_no_op_accs -l pd_no_op_accs -d 1 no_op_accs)
set proofs=($proofs -l mode_cntrl -l frame_prop -l no_op_accs -l fs_not_sc_seen -l fs_frame_gap -d 1 sc_init_frame_gap)
set proofs=($proofs -l mode_cntrl -l frame_prop -l no_op_accs -l fs_not_sc_seen -l sc_init_frame_gap -d 4 synched)
set proofs=($proofs -d 1 bad_echos_ascend)
set proofs=($proofs -l mode_cntrl -l sc_init_frame_gap -l fs_frame_gap -l frame_prop -l bad_echos_ascend -d 2 reint_to_least)
set proofs=($proofs -l reint_to_least -l fs_frame_gap -l synched -d 3 current_frame)
set proofs=($proofs -d 2 good_frame_update)


  set lemmas goal lm d k
  foreach word ($proofs)
    if ("${word}" == "-l") then
       set lm=lemma
    else if ("${lm}" == "lemma") then
       set lemmas=($lemmas "-l" $word)
       set lm
    else if ("${word}" == "-d") then
       set d=depth
    else if ("${d}" == "depth") then
       set k=("-d" $word)
       set d
    else 
       set goal=$word
       echo $prfcmd $k $lemmas $context $goal
       echo "=========== ${goal} ==========" >> $tracefile
       echo "" >> $tracefile
       $prfcmd $k $lemmas $context $goal >> $tracefile
       set lemmas goal lm d k
    endif
  end

