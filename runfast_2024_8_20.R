# Paper: "Faster Bank Runs"
# Code for simulations of Results 
#
### R packages needed
#library(nleqslv)# package numerical solution of a system of equations
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

# the configuration below yields optimal q between q_tilde and qmax
#(sigma=0.5)# speed of withdrawals => max q = 0.9 out of 1.0
#(rk = 0.15)# return on bank investments/lending
#(rd = 0.04)# interest paid on deposits
#(tau_bar = 2)# time when bailout cost is reduced to a fraction rho
#(rho = 0.9)
#(lambda = 0.1)#lambda (depositor's disutility from delay)
#(alpha=0.5)# exponent the bank's production/investment function
#(phi=0.2)# failure probability


# parameters needed for Subsection 4.3 (optimal q)
# # => This gives me q_tilde=0.9 & qopt=0.2
# (sigma=0.4)# speed of withdrawals => max q = 0.9 out of 1.0
# (rk = 0.2)# return on bank investments/lending
# (rd = 0.04)# interest paid on deposits
# (tau_bar = 2.0)# time when bailout cost is reduced to a fraction rho
# (rho = 0.9)
# (lambda = 0.5)#lambda (depositor's disutility from delay)
# (alpha=0.5)# exponent the bank's production/investment function
# (phi=0.1)# failure probability
# # verify Assumption 2
# tau_bar < 1/sigma + (1-rho)/lambda
# tau_bar > (1-rho)/lambda

# simulation for sigma = 0.2
(sigma=0.2)# speed of withdrawals
(rk = 0.2)# return on bank investments/lending
(rd = 0.04)# interest paid on deposits
(tau_bar = 2.0)# time when bailout cost is reduced to a fraction rho
(rho = 0.9)
(lambda = 0.5)#lambda (depositor's disutility from delay)
(alpha=0.5)# exponent the bank's production/investment function
(phi=0.1)# failure probability
# verify Assumption 2
tau_bar < 1/sigma + (1-rho)/lambda
tau_bar > (1-rho)/lambda

# Derived variables and variable definitions
(q_tilde = sigma*tau_bar - sigma*(1-rho)/lambda)# eq (9): threshold above which tb=tau_bar is optimal, Result 1.
(qmax = tau_bar*sigma)# eq (13) in paper
q_step = 0.005# resolution of the q vector (horizontal axis)
(q.vec = seq(0, 1, q_step))#full length

(tq.vec = q.vec/sigma)
# verify tau_bar > tq (depositors wait time)
which(q.vec == qmax)
(tau_bar-tq.vec)
(tau_bar-tq.vec[which(q.vec == qmax)])#
(tau_bar-tq.vec[which(q.vec == qmax-q_step)])#

# Welfare
(w_no_run.vec = rk*(1-q.vec)^alpha)# eq (14) in paper 
#
# top eq (15), q >= q_tilde
(w_run_high_q_temp.vec = rd -lambda*(tau_bar-tq.vec) *(1+rd-q.vec) -rho*(1+rd-q.vec))# 
w_run_high_q.vec = w_run_high_q_temp.vec# initialize
# now insert 0 for q < q_tilde
for (x in 1:length(q.vec)) {
  w_run_high_q.vec[x] = ifelse(q.vec[x] < q_tilde, 0, w_run_high_q_temp.vec[x])
}
w_run_high_q.vec
#
# bottom eq (15), q < q_tilde
(w_run_low_q_temp.vec = rd +0 -(1+rd-q.vec))# 
w_run_low_q.vec = w_run_low_q_temp.vec# initialize
# now insert 0 for q < q_tilde
for (x in 1:length(q.vec)) {
  w_run_low_q.vec[x] = ifelse(q.vec[x] >= q_tilde, 0, w_run_low_q_temp.vec[x])
}
w_run_low_q.vec
#
# define w_run for all q
w_run_low_q.vec
w_run_high_q.vec
#
(w_run.vec = w_run_low_q.vec + w_run_high_q.vec)
w_run_low_q.vec

# expected total welfare (low and high q combined)
(ew.vec = phi*w_run.vec + (1-phi)*w_no_run.vec)#
max(ew.vec)
which.max(ew.vec)
q.vec[which.max(ew.vec)]
(q_star = q.vec[which.max(ew.vec)])
# problem if max ew occurs at q > qmax. Fix it now
q_star = ifelse(q.vec[which.max(ew.vec)] > qmax, qmax, q.vec[which.max(ew.vec)])
q_star
q.vec
qmax
q_tilde
(profit= rk*(1-q.vec)^alpha - rd)# verify that profit is >0 at the optimal q
#
# insert NA in ew.vec for q > qmax
(ew_restr.vec = ew.vec)# initialize
for (x in 1:length(q.vec)) {
  ew_restr.vec[x] = ifelse(q.vec[x] > qmax, NA, ew.vec[x])
}
ew_restr.vec

# summary of (sigma = 0.2)
(sigma2 = sigma)
(tq2.vec = tq.vec)
# verify tq < tau_bar
tq2.vec[tq2.vec <= q_tilde/sigma2] <= tau_bar
(q2.vec = q.vec)
(q_tilde2 = q_tilde)
(q_star2 = q_star)
(qmax2 = qmax)
(ew2.vec = ew.vec)
(ew_restr2.vec = ew_restr.vec)
# end of sigma = 0.2

### simulation for sigma = 0.4
(sigma=0.4)# speed of withdrawals
(rk = 0.2)# return on bank investments/lending
(rd = 0.04)# interest paid on deposits
(tau_bar = 2.0)# time when bailout cost is reduced to a fraction rho
(rho = 0.9)
(lambda = 0.5)#lambda (depositor's disutility from delay)
(alpha=0.5)# exponent the bank's production/investment function
(phi=0.1)# failure probability
# verify Assumption 2
tau_bar < 1/sigma + (1-rho)/lambda
tau_bar > (1-rho)/lambda

# Derived variables and variable definitions
(q_tilde = sigma*tau_bar - sigma*(1-rho)/lambda)# eq (9): threshold above which tb=tau_bar is optimal, Result 1.
(qmax = tau_bar*sigma)# eq (13) in paper

(tq.vec = q.vec/sigma)
# verify tau_bar > tq (depositors wait time)
which(q.vec == qmax)
(tau_bar-tq.vec)
(tau_bar-tq.vec[which(q.vec == qmax)])#
(tau_bar-tq.vec[which(q.vec == qmax-q_step)])#

# Welfare
(w_no_run.vec = rk*(1-q.vec)^alpha)# eq (14) in paper 
#
# top eq (15), q >= q_tilde
(w_run_high_q_temp.vec = rd -lambda*(tau_bar-tq.vec) *(1+rd-q.vec) -rho*(1+rd-q.vec))# 
w_run_high_q.vec = w_run_high_q_temp.vec# initialize
# now insert 0 for q < q_tilde
for (x in 1:length(q.vec)) {
  w_run_high_q.vec[x] = ifelse(q.vec[x] < q_tilde, 0, w_run_high_q_temp.vec[x])
}
w_run_high_q.vec
#
# bottom eq (15), q < q_tilde
(w_run_low_q_temp.vec = rd +0 -(1+rd-q.vec))# 
w_run_low_q.vec = w_run_low_q_temp.vec# initialize
# now insert 0 for q >= q_tilde
for (x in 1:length(q.vec)) {
  w_run_low_q.vec[x] = ifelse(q.vec[x] >= q_tilde, 0, w_run_low_q_temp.vec[x])
}
w_run_low_q.vec
#
# define w_run for all q
w_run_low_q.vec
w_run_high_q.vec
#
(w_run.vec = w_run_low_q.vec + w_run_high_q.vec)

# expected total welfare (low and high q combined)
(ew.vec = phi*w_run.vec + (1-phi)*w_no_run.vec)#
max(ew.vec)
which.max(ew.vec)
q.vec[which.max(ew.vec)]
(q_star = q.vec[which.max(ew.vec)])
# problem if max ew occurs at q > qmax. Fix it now
q_star = ifelse(q.vec[which.max(ew.vec)] > qmax, qmax, q.vec[which.max(ew.vec)])
q_star
q.vec
qmax
q_tilde
(profit= rk*(1-q.vec)^alpha - rd)# verify that profit is >0 at the optimal q
## insert na in ew.vec for q > qmax
which(q.vec == qmax)
(ew_restr.vec = ew.vec)# initialize
for (x in 1:length(q.vec)) {
  ew_restr.vec[x] = ifelse(q.vec[x] > qmax, NA, ew.vec[x])
}
ew_restr.vec

# summary of (sigma = 0.4)
(sigma4 = sigma)
(tq4.vec = tq.vec)
# verify tq < tau_bar
tq4.vec[tq4.vec <= q_tilde/sigma4] <= tau_bar
(q4.vec = q.vec)
(q_tilde4 = q_tilde)
(q_star4 = q_star)
(qmax4 = qmax)
(ew4.vec = ew.vec)
(ew_restr4.vec = ew_restr.vec)
# end of sigma = 0.4

# data frame for the chart
(w.df = data.frame(q.vec, ew2.vec, ew_restr2.vec, ew4.vec, ew_restr4.vec ))
dim(w.df)
qmax2
qmax4
q_tilde2
q_tilde4
q_star2
q_star4
# Plot Figure xxx in the paper

ggplot(w.df, aes(x=q.vec)) +geom_line(aes(y=ew_restr2.vec), linetype="solid", size=1.2) +geom_line(aes(y=ew_restr4.vec), linetype="longdash", size=1.2, color="red") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +scale_x_continuous(breaks = seq(0,1,0.1)) +scale_y_continuous(breaks = seq(0.06, 0.09, 0.005)) +labs(x=TeX("Required liquidity ratio: $q$"), y=TeX("Expected total welfare: E$W(q)$")) +geom_vline(xintercept = qmax2, linetype = "dotted", size = 1) +geom_vline(xintercept = qmax4, color ="red", linetype = "dotted", size = 1) +annotate("text", x = qmax2, y = 0.062, label =TeX("$q^{max}_{\\sigma =0.2}$"), size = 7, color="black") +annotate("text", x = qmax4, y = 0.062, label =TeX("$q^{max}_{\\sigma =0.4}$"), size = 7, color="red") +annotate("text", x = q_tilde2, y = 0.064, label =TeX("$\\tilde{q}_{\\sigma =0.2}$"), size = 7, color="black") +geom_vline(xintercept = q_tilde2, linetype = "dotted", size = 1) +geom_vline(xintercept = q_tilde4, color ="red", linetype = "dotted", size = 1) +annotate("text", x = q_tilde4, y = 0.064, label =TeX("$\\tilde{q}_{\\sigma =0.4}$"), size = 7, color="red") +annotate("text", x = q_star2, y = 0.066, label =TeX("$q^{*}_{\\sigma =0.2}$"), size = 7, color="black") +geom_vline(xintercept = q_star4, color ="red", linetype = "dotted", size = 1) +annotate("text", x = q_star4, y = 0.066, label =TeX("$q^{*}_{\\sigma =0.4}$"), size = 7, color="red") +geom_segment(aes(x = 0.395, y = 0.07, xend = 0.2, yend = 0.07), arrow = arrow(length = unit(0.3, "cm")), color="red", size=1.0) +annotate("text", x = 0.3, y = 0.0707, label =TeX("$\\sigma \\uparrow$"), size = 7, color="red")

#+geom_line(aes(y=qhats2.vec), linetype="dashed", size=1.2, color="red")+ scale_x_continuous(breaks = seq(0.02,0.04,0.0025)) +scale_y_continuous(breaks = seq(0, 1, 0.1)) +labs(x=TeX("Failure probability: $\\phi$"), y=TeX("Optimal liquidity requirement: $q^F$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.03, y = 0.33, label =TeX("Earlier bailout time (lower $T_B^F$)"), size = 6, color="blue") +annotate("text", x = 0.025, y = 0.62, label =TeX("Faster withdrawals (higher $\\sigma$)"), size = 6, color="red")

### End of code 
# below I just test how ggplot handles NAs
# (x_test.vec = 1:4)
# (y_test.vec = c(1,2^2,3^2,4^2))
# (test.df = data.frame(x_test.vec, y_test.vec))
# ggplot(test.df, aes(x=x_test.vec)) + geom_line(aes(y=y_test.vec))
# # now insert na into y_test.vec
# y_test2.vec = c(1,2^2,3^2,NA)
# ggplot(test.df, aes(x=x_test.vec)) + geom_line(aes(y=y_test2.vec))
