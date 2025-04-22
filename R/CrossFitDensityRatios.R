CrossFitDensityRatios <- function(task, time, folds, control) {
    predictions <- list()
    for (v in 1:folds$numberFolds) {
        # Create validation subset from folds
        validation <- folds$validation(task$data, v)

        # Create pre-allocated vectors to store predictions
        predictionsMediator <- predictionsAStar <- predictionsAPrime <- predictionsCensoring <- vector("numeric", nrow(validation))

        # Create stacked data for training under a-prime shift
        stackedData <- task$stackData(folds$training(task$data, v), folds$training(task$shiftedUnderAPrime, v), time)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(stackedData, time-1)
        competingRiskFree <- task$competingRiskFree(stackedData, time-1)
        observed <- task$observed(stackedData, time, TRUE)

        # Subset data
        stackedData <- stackedData[outcomeFree & competingRiskFree & observed, ]

        # Create indicators for the subset of observations to use for predictions
        outcomeFreeValidation <- task$outcomeFree(validation, time-1)
        competingRiskFreeValidation <- task$competingRiskFree(validation, time-1)
        observedValidation <- task$observed(validation, time, TRUE)
        atRisk <- outcomeFreeValidation & competingRiskFreeValidation

        # Density ratio model predictions under the a-prime shift
        predictionsAPrime[atRisk & observedValidation] <-
            CrossFit(
            stackedData,
            P_a[outcomeFreeValidation & competingRiskFreeValidation & observedValidation, ],
            "lcmmtp_stack_indicator",
            c(task$variables$history("A", time), task$variables$treatment[time]),
            "binomial",
            control$learners_trt,
            control$folds_trt
        )

        # Create stacked data for training under a-star shift
        stackedData <- task$stackData(folds$training(task$data, v), folds$training(task$shiftedUnderAStar, v), time)

        # Subset data
        stackedData <- stackedData[outcomeFree & competingRiskFree & observed, ]

        # Density ratio model predictions under the a-star shift
        predictionsAStar[atRisk & observedValidation] <-
            CrossFit(
                stackedData,
                validation[atRisk & observedValidation, ],
                "lcmmtp_stack_indicator",
                c(task$variables$history("A", time), task$variables$treatment[time]),
                "binomial",
                control$learners_trt,
                control$folds_trt
            )

        # Create pooled data for predicting M=m
        augmentedData <- task$augment(folds$training(task$data, v), time)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(augmentedData, time-1)
        competingRiskFree <- task$competingRiskFree(augmentedData, time-1)
        observed <- task$observed(augmentedData, time, TRUE)

        # Create pseudo-outcome for training
        augmentedData[["lcmmtp_pseudo_m_fit"]] <- as.numeric(augmentedData[[g("lcmmtp_med_{t}")]] == augmentedData[[task$variables$mediator[time]]])

        # Estimate the probability M=m
        predictionsMediator[atRisk & observedValidation] <-
            CrossFit(
            augmentedData,
            validation[atRisk & observedValidation, ],
            "lcmmtp_pseudo_m_fit",
            c(task$variables$history("M", time), g("lcmmtp_med_{t}")),
            "binomial",
            control$learners_mediator,
            control$folds_mediator
        )

        # Create index for observation that were observed at the current time
        observedValidation <- task$observed(validation, time)

        # Create density ratios
        validation[[g("lcmmtp_Gp_A{t}")]] <- density_ratios(predictionsAPrime, atRisk, observedValidation)
        validation[[g("lcmmtp_Gs_A{t}")]] <- density_ratios(predictionsAStar, atRisk, observedValidation)
        validation[[g("lcmmtp_G_M{t}")]] <- G(P_a[[task$variables$mediator[time]]], predictionsMediator, validation[[g("lcmmtp_med_{t}")]], atRisk, observed)

        predictions[[v]] <- validation
    }

    predictions <- Reduce(rbind, predictions)
    data.table::setorder(predictions, "lcmmtp_ID")

    task$augmented <- predictions
}
