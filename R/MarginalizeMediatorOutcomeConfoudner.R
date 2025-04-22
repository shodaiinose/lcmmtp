MarginalizeMediatorOutcomeConfounder <- function(task, time, folds, d_prime, control) {
    predictions <- list()
    for (v in 1:folds$numberFolds) {
        # Create augmented training subset from folds
        training <- folds$training(task$augmented, v)
        # Create augmented validation subset from folds
        validation <- folds$validation(task$augmented, v)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(training, time-1)
        competingRiskFree <- task$competingRiskFree(training, time-1)
        observed <- task$observed(training, time, TRUE)

        # Subset data
        training <- training[outcomeFree & competingRiskFree & observed, ]

        # Create indicators for the subset of observations to use for predictions
        outcomeFreeValidation <- task$outcomeFree(validation, time-1)
        competingRiskFreeValidation <- task$competingRiskFree(validation, time-1)
        observedValidation <- task$observed(validation, time, TRUE)
        atRisk <- outcomeFreeValidation & competingRiskFreeValidation

        # Regress pseudo-outcome on treatment, parents of treatment
        validation[[g("lcmmtp_Q_Z{time}")]][atRisk & observedValidation] <- CrossFit(
            training,
            task$shiftTreatment(validation[atRisk & observedValidation, ], task$variables$treatment[time], task$variables$censoring[time], d_prime),
            g("lcmmtp_D_L{time}"),
            c(g("lcmmtp_med_{time:task$variables$timeHorizon}"), task$variables$history("A", time), task$variables$treatment[time]),
            "continuous",
            control$learners_QL,
            control$folds_QL
        )

        # Assign deterministic probabilities based on already experiencing the outcome or the competing risk
        validation[[g("lcmmtp_Q_Z{time}")]][!outcomeFreeValidation] <- 0
        validation[[g("lcmmtp_Q_Z{time}")]][!competingRiskFreeValidation] <- 1

        predictions[[v]] <- validation
    }

    predictions <- Reduce(rbind, predictions)
    data.table::setorder(predictions, "lcmmtp_ID")
    task$augmented <- predictions

    return(invisible())
}
