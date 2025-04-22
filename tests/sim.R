library(simcausal)
library(dplyr)

# dag <-
#     DAG.empty() +
#     # time 1
#     node("L1", distr = "rbern", prob = 0.5) +
#     node("A1", distr = "rbern", prob = 0.4 + L1*0.1) +
#     # node("A1", distr = "rbern", prob = 1) +
#     node("C1", distr = "rbern", prob = 0, EFU = TRUE) +
#     node("Z1", distr = "rbern", prob = 0.33 - A1*0.1 + L1*0.1) +
#     node("M1", distr = "rbern", prob = 0.6 - Z1*0.2 - A1*0.1 + L1*0.1) +
#     node("D2", distr = "rbern", prob = 0.01 + M1*0.05 + Z1*0.02 + A1*0.02 - L1*0.001, EFU = TRUE) +
#     node("Y2", distr = "rbern", prob = 0.3 + M1*0.05 + Z1*0.02 + A1*0.2 - L1*0.001, EFU = TRUE) +
#     # time 2
#     node("L2", distr = "rbern", prob = 0.5) +
#     node("A2", distr = "rbern", prob = 0.4 + L2*0.1) +
#     # node("A2", distr = "rbern", prob = 1) +
#     node("C2", distr = "rbern", prob = 0, EFU = TRUE) +
#     node("Z2", distr = "rbern", prob = 0.33 - A2*0.1 + L2*0.1) +
#     node("M2", distr = "rbern", prob = 0.6 - Z2*0.2 - A2*0.1 + L2*0.1) +
#     node("D3", distr = "rbern", prob = 0.01 + M2*0.05 + Z2*0.02 + A2*0.02 - L2*0.001, EFU = TRUE) +
#     node("Y3", distr = "rbern", prob = 0.3 + M2*0.05 + Z2*0.02 + A2*0.2 - L2*0.001, EFU = TRUE) +
#     # time 3
#     node("L3", distr = "rbern", prob = 0.5) +
#     node("A3", distr = "rbern", prob = 0.4 + L3*0.1) +
#     # node("A3", distr = "rbern", prob = 1) +
#     node("C3", distr = "rbern", prob = 0, EFU = TRUE) +
#     node("Z3", distr = "rbern", prob = 0.33 - A3*0.1 + L3*0.1) +
#     node("M3", distr = "rbern", prob = 0.6 - Z3*0.3 - A3*0.1 + L3*0.1) +
#     node("D4", distr = "rbern", prob = 0.01 + M3*0.05 + Z3*0.03 + A3*0.03 - L3*0.001, EFU = TRUE) +
#     node("Y4", distr = "rbern", prob = 0.3 + M3*0.05 + Z3*0.03 + A3*0.3 - L3*0.001, EFU = TRUE)

dag <-
    DAG.empty() +
    # time 1
    node("L1", distr = "rbern", prob = 0.5) +
    node("A1", distr = "rbern", prob = 0.4 + L1*0.1) +
    # node("A1", distr = "rbern", prob = 1) +
    # node("C1", distr = "rbern", prob = 0, EFU = TRUE) +
    node("M1", distr = "rbern", prob = 0.6 - A1*0.1 + L1*0.1) +
    node("D2", distr = "rbern", prob = 0.01 + M1*0.05 + A1*0.02 - L1*0.001) +
    node("Y2", distr = "rbern", prob = D2*0 + (1-D2)*(0.3 + M1*0.05 + A1*0.2 - L1*0.001), EFU = TRUE) +
    # time 2
    node("L2", distr = "rbern", prob = 0.5) +
    node("A2", distr = "rbern", prob = 0.4 + L2*0.1) +
    # node("A2", distr = "rbern", prob = 1) +
    # node("C2", distr = "rbern", prob = 0, EFU = TRUE) +
    node("M2", distr = "rbern", prob = D2*0 + (1-D2)*(0.6 - A2*0.1 + L2*0.1)) +
    node("D3", distr = "rbern", prob = D2*1 + (1-D2)*(0.01 + M2*0.05 + A2*0.02 - L2*0.001)) +
    node("Y3", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M2*0.05 + A2*0.2 - L2*0.001), EFU = TRUE) +
    # time 3
    node("L3", distr = "rbern", prob = 0.5) +
    node("A3", distr = "rbern", prob = 0.4 + L3*0.1) +
    # node("A3", distr = "rbern", prob = 1) +
    # node("C3", distr = "rbern", prob = 0, EFU = TRUE) +
    node("M3", distr = "rbern", prob = D3*0 + (1-D3)*(0.6 - A3*0.1 + L3*0.1)) +
    node("D4", distr = "rbern", prob = D3*1 + (1-D3)*(0.01 + M3*0.05 + A3*0.03 - L3*0.001)) +
    node("Y4", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M3*0.05 + A3*0.3 - L3*0.001), EFU = TRUE)

dag <- set.DAG(dag)

data <- sim(dag, n = 1e4, LTCF = "Y")

data <- data |>
    mutate(
        # across(starts_with("C"), ~ifelse(. == 1, 0, 1)),
        # C2 = ifelse(C1 == 0, C1, C2),
        # C3 = ifelse(C2 == 0, C2, C3),
        D3 = ifelse(D2 == 1, D2, D3),
        D4 = ifelse(D3 == 1, D3, D4),
        Y2 = ifelse(D2 == 1, 0, Y2),
        Y3 = ifelse(D3 == 1, 0, Y3),
        Y4 = ifelse(D4 == 1, 0, Y4),
        Y3 = ifelse(Y2 == 1, Y2, Y3),
        Y4 = ifelse(Y3 == 1, Y3, Y4),
        M2 = ifelse(Y2 == 1, 0, M2),
        M3 = ifelse(Y3 == 1, 0, M3)
    )

truth1 <- 0.16959
truth0 <- 0.34626

# library(devtools)
#
# load_all()

vars <- lcmmtp_variables$new(
    L = list(c("L1"), c("L2"), c("L3")),
    A = c("A1", "A2", "A3"),
    # Z = list(c("Z1"), c("Z2"), c("Z3")),
    Z = list(NULL, NULL, NULL),
    M = c("M1", "M2", "M3"),
    Y = c("Y2", "Y3", "Y4"),
    D = c("D2", "D3", "D4")
)

# vars <- lcmmtp_variables$new(
#     L = list(c("L1"), c("L2")),
#     A = c("A1", "A2"),
#     # Z = list(c("Z1"), c("Z2"), c("Z3")),
#     Z = list(NULL, NULL),
#     M = c("M1", "M2"),
#     Y = c("Y2", "Y3"),
#     D = c("D2", "D3")
# )

# vars <- lcmmtp_variables$new(
#     L = list(c("L1")),
#     A = c("A1"),
#     # Z = list(c("Z1"), c("Z2"), c("Z3")),
#     Z = list(NULL),
#     M = c("M1"),
#     Y = c("Y2"),
#     D = c("D2")
# )

pop <- po("modelmatrix", formula = ~ .^2)
d_ap <- function(data, trt) rep(1, length(data[[trt]]))
d_as <- function(data, trt) rep(1, length(data[[trt]]))
lcmmtp(data, vars, d_ap, d_as,
       control = .lcmmtp_control(folds = 1))

d_ap <- function(data, trt) data[[trt]]
lcmmtp(data, vars, d_ap, d_ap, control = .lcmmtp_control(folds = 1))

