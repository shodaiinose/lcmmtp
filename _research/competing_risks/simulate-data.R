library(simcausal)

# Y_t ~ A, L_{t-1}, M_{t-1}
#   Pr(Y_t = 1 | D_t == 1) = 0
#   Pr(Y_t = 1 | Y_{t-1} == 1) = 1
# D_t ~ A, L_{t-1}, M_{t-1}
#   Pr(D_t = 1 | Y_{t-1} == 1) = 0
#   Pr(D_t = 1 | D_{t-1} == 1) = 1
# M_t ~ A, L_t
# L_t ~ A, M_{t-1} for t > 0
# A ~ L_0
# L_0 ~ U_l
competing_risks_dag <- 
  DAG.empty() +
  node("D", t = 0, distr = "rbern", prob = 0) + 
  node("Y", t = 0, distr = "rbern", prob = 0) + 
  node("L", t = 0, distr = "rbern", prob = 0.5) +
  node("A", t = 0, distr = "rbern", prob = plogis(0.1 + .5*L[0])) + 
  node("M", t = 0, distr = "rbern", prob = 0.25 + 0.1*A[0] + 0.2*L[0]) + 
  node("D", t = 1:4, distr = "rbern", prob = ifelse(
    Y[t-1] == 1, 0, ifelse(
      D[t-1] == 1, 1, plogis(-3 + 0.75 * (A[0] == 1) + 0.15*L[t-1] + 0.2*M[t-1])
    )
  )) +  
  node("Y", t = 1:4, distr = "rbern", 
       prob = ifelse(
        D[t] == 1, 0, ifelse(
          Y[t-1] == 1, 1, plogis(-4 + 2*(A[0] == 1) - 0.25*L[t-1] - 0.3*M[t-1])
        )
        )) + 
  node("L", t = 1:3, distr = "rbern", prob = 0.3 + 0.05*A[0] + 0.1*M[t-1]) + 
  node("M", t = 1:3, distr = "rbern", prob = 0.25 + 0.1*A[0] + 0.2*L[t])

competing_risks_dag <- set.DAG(competing_risks_dag)

foo_data <- sim(competing_risks_dag, n = 1e3)
