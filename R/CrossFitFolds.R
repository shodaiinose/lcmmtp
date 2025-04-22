CrossFitFolds <- R6::R6Class(
    "CrossFitFolds",
    cloneable = FALSE,
    public = list(
        folds = NULL,
        numberFolds = NULL,
        initialize = function(n, numberFolds, clusterIds = NULL) {
            self$folds <- origami::make_folds(n, V = numberFolds, cluster_ids = clusterIds)

            if (numberFolds == 1) {
                self$folds[[1]]$training_set <- self$folds[[1]]$validation_set
            }

            self$numberFolds <- numberFolds
        },

        # Get training data from a given fold index
        training = function(data, index) {
            data[data$`lcmmtp_row_index` %in% self$folds[[index]]$training_set, ]
        },

        # Get validation data from a given fold index
        validation = function(data, index) {
            data[data$`lcmmtp_row_index` %in% self$folds[[index]]$validation_set, ]
        }
    )
)
