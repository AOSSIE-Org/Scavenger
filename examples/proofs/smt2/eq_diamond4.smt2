(set .c1 (input :conclusion (#1:(and #2:(or #3:(and #4:(= y0 x0) #5:(= y0 x1)) #6:(and #7:(= z0 x0) #8:(= z0 x1))) #9:(or #10:(and #11:(= y1 x1) #12:(= y1 x2)) #13:(and #14:(= z1 x1) #15:(= z1 x2))) #16:(or #17:(and #18:(= x2 y2) #19:(= x3 y2)) #20:(and #21:(= z2 x2) #22:(= z2 x3))) (not #23:(= x3 x0))))))
(set .c2 (and :clauses (.c1) :conclusion (#2)))
(set .c3 (and_pos :conclusion ((not #3) #4)))
(set .c4 (and_pos :conclusion ((not #3) #5)))
(set .c5 (and_pos :conclusion ((not #6) #7)))
(set .c6 (and_pos :conclusion ((not #6) #8)))
(set .c7 (or :clauses (.c2) :conclusion (#3 #6)))
(set .c8 (and :clauses (.c1) :conclusion (#9)))
(set .c9 (and_pos :conclusion ((not #10) #11)))
(set .c10 (and_pos :conclusion ((not #10) #12)))
(set .c11 (and_pos :conclusion ((not #13) #14)))
(set .c12 (and_pos :conclusion ((not #13) #15)))
(set .c13 (or :clauses (.c8) :conclusion (#10 #13)))
(set .c14 (and :clauses (.c1) :conclusion (#16)))
(set .c15 (and_pos :conclusion ((not #17) #18)))
(set .c16 (and_pos :conclusion ((not #17) #19)))
(set .c17 (and_pos :conclusion ((not #20) #21)))
(set .c18 (and_pos :conclusion ((not #20) #22)))
(set .c19 (or :clauses (.c14) :conclusion (#17 #20)))
(set .c20 (and :clauses (.c1) :conclusion ((not #23))))
(set .c21 (eq_transitive :conclusion ((not #19) (not #18) (not #12) (not #11) (not #8) (not #7) #23)))
(set .c22 (resolution :clauses (.c21 .c20) :conclusion ((not #19) (not #18) (not #12) (not #11) (not #8) (not #7))))
(set .c23 (resolution :clauses (.c22 .c10 .c9) :conclusion ((not #19) (not #18) (not #8) (not #7) (not #10))))
(set .c24 (eq_transitive :conclusion ((not #19) (not #18) (not #15) (not #14) (not #8) (not #7) #23)))
(set .c25 (resolution :clauses (.c24 .c20) :conclusion ((not #19) (not #18) (not #15) (not #14) (not #8) (not #7))))
(set .c26 (resolution :clauses (.c25 .c12 .c11 .c13 .c23 .c16 .c15) :conclusion ((not #8) (not #7) (not #17))))
(set .c27 (eq_transitive :conclusion ((not #22) (not #21) (not #15) (not #14) (not #8) (not #7) #23)))
(set .c28 (resolution :clauses (.c27 .c20) :conclusion ((not #22) (not #21) (not #15) (not #14) (not #8) (not #7))))
(set .c29 (eq_transitive :conclusion ((not #22) (not #21) (not #12) (not #11) (not #8) (not #7) #23)))
(set .c30 (resolution :clauses (.c29 .c20) :conclusion ((not #22) (not #21) (not #12) (not #11) (not #8) (not #7))))
(set .c31 (resolution :clauses (.c30 .c10 .c9) :conclusion ((not #22) (not #21) (not #8) (not #7) (not #10))))
(set .c32 (resolution :clauses (.c28 .c12 .c11 .c13 .c31 .c18 .c17 .c19 .c26 .c6 .c5) :conclusion ((not #6))))
(set .c33 (resolution :clauses (.c7 .c32) :conclusion (#3)))
(set .c34 (resolution :clauses (.c3 .c33) :conclusion (#4)))
(set .c35 (resolution :clauses (.c4 .c33) :conclusion (#5)))
(set .c36 (eq_transitive :conclusion ((not #22) (not #21) (not #15) (not #14) (not #5) (not #4) #23)))
(set .c37 (resolution :clauses (.c36 .c34 .c35 .c20) :conclusion ((not #22) (not #21) (not #15) (not #14))))
(set .c38 (eq_transitive :conclusion ((not #19) (not #18) (not #15) (not #14) (not #5) (not #4) #23)))
(set .c39 (resolution :clauses (.c38 .c34 .c35 .c20) :conclusion ((not #19) (not #18) (not #15) (not #14))))
(set .c40 (resolution :clauses (.c39 .c16 .c15) :conclusion ((not #15) (not #14) (not #17))))
(set .c41 (resolution :clauses (.c37 .c18 .c17 .c19 .c40 .c12 .c11) :conclusion ((not #13))))
(set .c42 (resolution :clauses (.c13 .c41) :conclusion (#10)))
(set .c43 (resolution :clauses (.c9 .c42) :conclusion (#11)))
(set .c44 (resolution :clauses (.c10 .c42) :conclusion (#12)))
(set .c45 (eq_transitive :conclusion ((not #22) (not #21) (not #12) (not #11) (not #5) (not #4) #23)))
(set .c46 (resolution :clauses (.c45 .c34 .c35 .c43 .c44 .c20) :conclusion ((not #22) (not #21))))
(set .c47 (resolution :clauses (.c46 .c18 .c17) :conclusion ((not #20))))
(set .c48 (resolution :clauses (.c19 .c47) :conclusion (#17)))
(set .c49 (resolution :clauses (.c15 .c48) :conclusion (#18)))
(set .c50 (resolution :clauses (.c16 .c48) :conclusion (#19)))
(set .c51 (eq_transitive :conclusion ((not #19) (not #18) (not #12) (not #11) (not #5) (not #4) #23)))
(set .c52 (resolution :clauses (.c51 .c34 .c35 .c43 .c44 .c49 .c50 .c20) :conclusion ()))