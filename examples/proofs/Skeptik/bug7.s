n0 = ({ ⊢ b, a}.{b ⊢ d})
u0 = { ⊢ b}
n1 = ({b, a ⊢ c}.u0)
u1 = ((n0.{d, b ⊢ e}).u0)
n2 = {c, e ⊢ }
qed= (({e ⊢ c}.(n1.u1)).((u1.n2).{a, c ⊢ }))