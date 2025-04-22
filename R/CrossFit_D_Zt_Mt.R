CrossFit_D_Zt_Mt <- function(x, d_prime, d_star, t, Folds, control) {
    if (t == x$vars$tau) {
        x$augmented[[g("lcmmtp_D_M{t+1}")]] <- 1
        x$augmented[[g("lcmmtp_Q_M{t+1}")]] <- 1
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        # Create augmented versions of the training and prediction data
        Tr_a <- Folds$Tr(x$augmented, v)
        P_a  <- Folds$P(x$augmented, v)

        # Indicator for already experiencing the outcome
        y1 <- x$at_risk_N(Tr_a, t-1)
        y1v <- x$at_risk_N(P_a, t-1)
        # Indicator for having experienced the competing risk
        d0 <- x$at_risk_D(Tr_a, t-1)
        d0v <- x$at_risk_D(P_a, t-1)
        # Indicator for being censored
        c1 <- x$observed(Tr_a, t)
        c1v <- x$observed(P_a, t)
        # Filter the training data to only include those who are at risk
        Tr_a <- Tr_a[y1 & d0 & c1, ]

        # Regress pseudo-outcome on treatment, parents of treatment
        P_a[[g("lcmmtp_Q_Z{t}")]][y1v & d0v & c1v] <- CrossFit(
            Tr_a,
            x$shift_trt(P_a[y1v & d0v & c1v, ], x$vars$A[t], x$vars$cens[t], d_prime),
            g("lcmmtp_D_L{t}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"), x$vars$history("A", t), x$vars$A[t]),
            "continuous",
            control$learners_QL,
            control$folds_QL
        )

        # Assign deterministic probabilities based on already experiencing the outcome or the competing risk
        P_a[[g("lcmmtp_Q_Z{t}")]][!y1v] <- 0
        P_a[[g("lcmmtp_Q_Z{t}")]][!d0v] <- 1

        # Recreate augmented version of the training data
        Tr_a <- Folds$Tr(x$augmented, v)

        # Indicator for having experienced the competing risk
        d0 <- x$at_risk_D(Tr_a, t-2)
        d0v <- x$at_risk_D(P_a, t-2)

        # Filter the training data to only include those who are at risk
        Tr_a <- Tr_a[y1 & d0 & c1, ]

        # Create outcome variable
        Tr_a[[g("lcmmtp_D_M{t+1}")]] <-
            (Tr_a[[g("lcmmtp_med_{t}")]] == Tr_a[[x$vars$M[t]]]) * Tr_a[[g("lcmmtp_D_M{t+1}")]]

        # Estimate the probability of M = m in the pooled data
        P_a[[g("lcmmtp_Q_M{t}")]][y1v & d0v & c1v] <- CrossFit(
            Tr_a,
            x$shift_trt(P_a[y1v & d0v & c1v, ], x$vars$A[t], x$vars$cens[t], d_star),
            g("lcmmtp_D_M{t+1}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"), x$vars$history("A", t), x$vars$A[t]),
            ifelse(t == x$vars$tau, "binomial", "continuous"),
            control$learners_QM,
            control$folds_QM
        )

        # Assign deterministic probabilities for the value of the history of M
        # If the entire history of M is zero, the probability is 1
        # If the history of M contains any non-zero, the probability is 0
        historyIsZero <- apply(P_a[, g("lcmmtp_med_{t:x$vars$tau}"), drop = FALSE] == 0, 1, prod)
        P_a[[g("lcmmtp_Q_M{t}")]][!historyIsZero & !(y1v*d0v)] <- 0
        P_a[[g("lcmmtp_Q_M{t}")]][historyIsZero & !(y1v*d0v)] <- 1

        # Create pseudo outcomes based on the efficient influence function
        P_a[[g("lcmmtp_D_Z{t}")]] <- D_Zt(P_a, t, x$vars$tau)
        P_a[[g("lcmmtp_D_M{t}")]] <- D_Mt(P_a, t, x$vars$tau, x$vars$M)

        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    x$augmented <- cfd
}
