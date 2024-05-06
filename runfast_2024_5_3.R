# Paper: "Faster Bank Runs"
# Code for simulations of Results 
# Code for Figure [not used in paper, used Mathcha instead]
# verifying simulating max welfare problem eq (9)
### Line 169, Section 4 extension start simulations (may require different parameters)
#
### R packages needed
#library(nleqslv)# package numerical solution of a system of equations
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

# parameters of the base model:
#(sigma.vec = seq(0.1, 0.4, 0.01))
(sig = 0.1)
(phi.vec = seq(0.02, 0.06, 0.01))
#(tb_slow = 12.9)# TB in paper
(tb_fast = 1)
r_k = 0.18 #R_k in paper
# verify r_k is sufficiently high
r_k > phi.vec/(1-phi.vec)
r_d = 0.04 #R_d in paper
(lambda = 4)
(delta = 2)

(qhats = 1+r_d+sig*tb_fast -((r_k*(1-phi.vec) - phi.vec)/(phi.vec*lambda* delta*tb_fast))^(1/(delta-1)))

# verify that tb_fast < qhats/sig
(qhats/sig)

################
# parameters of the base model:
#(sigma.vec = seq(0.1, 0.4, 0.01))
(sig = 0.1)
(phi.vec = seq(0.02, 0.04, 0.0001))
#(tb_slow = 12.9)# TB in paper
(tb_fast = 1)
r_k = 0.18 #R_k in paper
# verify r_k is sufficiently high
r_k > phi.vec/(1-phi.vec)
r_d = 0.04 #R_d in paper
(lambda = 4)
(delta = 2)

(qhats0.vec = 1+r_d+sig*tb_fast -((r_k*(1-phi.vec) - phi.vec)/(phi.vec*lambda* delta*tb_fast))^(1/(delta-1)))
#
length(qhats0.vec)
# verify that tb_fast < qhats/sig
(qhats0.vec/sig)

# 2nd graph: faster bailout (lower tb_fast)
tb_fast
(tb_fast1 = 0.9)
(qhats1.vec = 1+r_d+sig*tb_fast1 -((r_k*(1-phi.vec) - phi.vec)/(phi.vec*lambda* delta*tb_fast1))^(1/(delta-1)))

# 3rd graph: faster withdrawal rate (higher sig)
sig
(sig2 = 0.2)
(qhats2.vec = 1+r_d+sig2*tb_fast -((r_k*(1-phi.vec) - phi.vec)/(phi.vec*lambda* delta*tb_fast))^(1/(delta-1)))


# Prepare data frame
(qhats.df = data.frame(phi.vec, qhats0.vec, qhats1.vec, qhats2.vec))

# Plot Figure xxx in the paper
ggplot(qhats.df, aes(x=phi.vec)) +geom_line(aes(y=qhats0.vec), linetype="solid", size=1.2) +geom_line(aes(y=qhats1.vec), linetype="dotdash", size=1.2, color="blue") +geom_line(aes(y=qhats2.vec), linetype="dashed", size=1.2, color="red")+ scale_x_continuous(breaks = seq(0.02,0.04,0.0025)) + scale_y_continuous(breaks = seq(0, 1, 0.1)) +labs(x=TeX("Failure probability: $\\phi$"), y=TeX("Optimal liquidity requirement: $q^F$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.03, y = 0.33, label =TeX("Earlier bailout time (lower $T_B^F$)"), size = 6, color="blue") +annotate("text", x = 0.025, y = 0.62, label =TeX("Faster withdrawals (higher $\\sigma$)"), size = 6, color="red")

### End of code 



