CrossFit_D_Lt <- function(x, t, Folds, control) {
    if (t == x$vars$tau) {
        x$augmented[[g("lcmmtp_D_Z{t+1}")]] <- x$augmented[[x$vars$Y]]
        x$augmented[[g("lcmmtp_Q_Z{t+1}")]] <- x$augmented[[x$vars$Y]]
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr   <- Folds$Tr(x$augmented, v)
        Tr_a <- x$augment(Tr, t)
        P_a  <- x$augment(Folds$P(x$augmented, v), t)

        y1 <- x$at_risk_N(Tr_a, t-1)
        d0 <- x$at_risk_D(Tr_a, t-1)
        c1 <- x$observed(Tr_a, t)
        Tr_a <- Tr_a[y1 & d0 & c1, ]

        y1v <- x$at_risk_N(P_a, t-1)
        d0v <- x$at_risk_D(P_a, t-1)
        ro <- y1v & d0v & x$observed(P_a, t, T)

        g_Mt <- g_Ast <- g_Apt <- g_Ct <- vector("numeric", nrow(P_a))

        P_a[[g("lcmmtp_Q_L{t}")]][ro] <- CrossFit(
            Tr_a[Tr_a[[g("lcmmtp_med_{t}")]] == Tr_a[[x$vars$M[t]]], ],
            P_a[ro, ],
            g("lcmmtp_D_Z{t+1}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"), x$vars$history("M", t)),
            ifelse(t == x$vars$tau, x$type, "continuous"),
            control$learners_QZ,
            control$folds_QZ
        )

        P_a[[g("lcmmtp_Q_L{t}")]][!y1] <- 0
        P_a[[g("lcmmtp_Q_L{t}")]][!d0] <- 1

        # Tr <- Tr[x$at_risk(Tr, t), ]

        Tr_Ap <- x$stack_data(Folds$Tr(x$data, v), Folds$Tr(x$shifted_aprime, v), t)
        y1 <- x$at_risk_N(Tr_Ap, t-1)
        d0 <- x$at_risk_D(Tr_Ap, t-1)
        c1 <- x$observed(Tr_Ap, t, TRUE)
        Tr_Ap <- Tr_Ap[y1 & d0 & c1, ]

        g_Apt[ro] <- CrossFit(
            Tr_Ap,
            P_a[ro, ],
            "tmp_lcmmtp_stack_indicator",
            c(x$vars$history("A", t), x$vars$A[t]),
            "binomial",
            control$learners_trt,
            control$folds_trt
        )

        Tr_As <- x$stack_data(Folds$Tr(x$data, v), Folds$Tr(x$shifted_astar, v), t)
        y1 <- x$at_risk_N(Tr_As, t-1)
        d0 <- x$at_risk_D(Tr_As, t-1)
        c1 <- x$observed(Tr_As, t, TRUE)
        Tr_As <- Tr_As[y1 & d0 & c1, ]

        g_Ast[ro] <- CrossFit(
            Tr_As,
            P_a[ro, ],
            "tmp_lcmmtp_stack_indicator",
            c(x$vars$history("A", t), x$vars$A[t]),
            "binomial",
            control$learners_trt,
            control$folds_trt
        )

        # Create pooled data for g_Mt fit
        m_fit_data <- x$augment(Folds$Tr(x$data, v), t)
        y1 <- x$at_risk_N(m_fit_data, t-1)
        d0 <- x$at_risk_D(m_fit_data, t-1)
        c1 <- x$observed(m_fit_data, t, TRUE)
        m_fit_data <- m_fit_data[y1 & d0 & c1, ]

        m_fit_data[["lcmmtp_pseudo_m_fit"]] <-
            as.numeric(m_fit_data[[g("lcmmtp_med_{t}")]] == m_fit_data[[x$vars$M[t]]])

        g_Mt[ro] <- CrossFit(
            m_fit_data,
            P_a[ro, ],
            "lcmmtp_pseudo_m_fit",
            c(x$vars$history("M", t), g("lcmmtp_med_{t}")),
            "binomial",
            control$learners_mediator,
            control$folds_mediator
        )

        r <- x$at_risk_N(P_a, t-1) & x$at_risk_D(P_a, t-1)
        o <- x$observed(P_a, t)

        P_a[[g("lcmmtp_Gp_A{t}")]] <- density_ratios(g_Apt, r, o)
        P_a[[g("lcmmtp_Gs_A{t}")]] <- density_ratios(g_Ast, r, o)
        P_a[[g("lcmmtp_G_M{t}")]] <- G(P_a[[x$vars$M[t]]],
                                       g_Mt,
                                       P_a[[g("lcmmtp_med_{t}")]],
                                       r, o)

        P_a[[g("lcmmtp_D_L{t}")]] <- D_Lt(P_a, t, x$vars$tau)
        # P_a[[g("lcmmtp_D_L{t}")]] <- P_a[[g("lcmmtp_Q_L{t}")]]
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    x$augmented <- cfd
}
