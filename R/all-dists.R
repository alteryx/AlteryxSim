#' function to make list of all distributions and relevant info
#' 
#' @return list of distributions - each distribution has elements 'distribution', 'name', 'bounds', and 'isInt'
allDistributions <- function() {

  # Distribution Format
  #   dist = list(
  #     distribution = "nameInR",
  #     name = "colloquialName",
  #     params = list(
  #       param1 = list(
  #         name = "param1Name",
  #         bounds = c(lower, upper),
  #         isInt = whether param can only take int values
  #         ),
  #       param2 = ...,
  #       param3 = ...
  #       ),
  #     bounds = c(lower bound of support, upper bound of support),
  #     discrete = only integer outputs?
  #     )

  list(
    # beta = list(
    #   distribution = "beta",
    #   name = "beta",
    #   params = list(
    #     param1 = list(
    #       name = "shape1",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "shape2",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    binomial = list(
      distribution = "binom",
      name = "binomial",
      params = list(
        param1 = list(
          name = "size",
          bounds = c(0, Inf),
          isInt = TRUE
          ),
        param2 = list(
          name = "prob",
          bounds = c(0,1),
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(0, Inf),
      discrete = TRUE
      ),

    # cauchy = list(
    #   distribution = "cauchy",
    #   name = "cauchy",
    #   params = list(
    #     param1 = list(
    #       name = "location",
    #       bounds = c(-Inf, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "scale",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    # chi-square = list(
    #   distribution = "chisq",
    #   name = "chi-square",
    #   params = list(
    #     param1 = list(
    #       name = "df",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = NULL,
    #     param3 = NULL
    #     )
    #   ),

    # exponential = list(
    #   distribution = "exp",
    #   name = "exponential",
    #   params = list(
    #     param1 = list(
    #       name = "rate",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = NULL,
    #     param3 = NULL
    #     )
    #   ),

    # f = list(
    #   distribution = "f",
    #   name = "f",
    #   params = list(
    #     param1 = list(
    #       name = "df1",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "df2",
    #       bounds = c(0, Inf),
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    gamma = list(
      distribution = "gamma",
      name = "gamma",
      params = list(
        param1 = list(
          name = "shape",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "scale",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(.0000001, Inf),
      discrete = FALSE
      ),

    geometric = list(
      distribution = "geom",
      name = "geometric",
      params = list(
        param1 = list(
          name = "prob",
          bounds = c(0,1),
          isInt = FALSE
          ),
        param2 = NULL,
        param3 = NULL
        ),
      bounds = c(0, Inf),
      discrete = TRUE
      ),

    # hypergeometric = list(
    #   distribution = "hyper",
    #   name = "hypergeometric",
    #   params = list(
    #     param1 = list(
    #       name = "m",
    #       bounds = c(0, Inf),
    #       isInt = TRUE
    #       ),
    #     param2 = list(
    #       name = "n",
    #       bounds = c(0, Inf),
    #       isInt = TRUE,
    #       ),
    #     param3 = list(
    #       name = "k",
    #       bounds = c(0, Inf) #really, c(0, m+n)
    #       )
    #     )
    #   ),

    # logistic = list(
    #   distribution = "logis",
    #   name = "logistic",
    #   params = list(
    #     param1 = list(
    #       name = "location",
    #       bounds = c(-Inf, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "scale",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    lognormal = list(
      distribution = "lnorm",
      name = "lognormal",
      params = list(
        param1 = list(
          name = "meanlog",
          bounds = c(-Inf, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "sdlog",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(.0000001, Inf),
      discrete = FALSE
      ),

    # negative_binomial = list(
    #   distribution = "nbinom",
    #   name = "negative_binomial",
    #   params = list(
    #     param1 = list(
    #       name = "size",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "prob",
    #       bounds = c(0,1),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    normal = list(
      distribution = "norm",
      name = "normal",
      params = list(
        param1 = list(
          name = "mean",
          bounds = c(-Inf, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "sd",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(-Inf, Inf),
      discrete = FALSE
      ),

    pareto = list(
      distribution = "pareto",
      name = "pareto",
      params = list(
        param1 = list(
          name = "shape",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "scale",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(.0000001, Inf),
      discrete = FALSE
      ),

    poisson = list(
      distribution = "pois",
      name = "poisson",
      params = list(
        param1 = list(
          name = "lambda",
          bounds = c(0, Inf),
          isInt = FALSE
          ),
        param2 = NULL,
        param3 = NULL
        ),
      bounds = c(0, Inf),
      discrete = TRUE
      ),

    # student_t = list(
    #   distribution = "t",
    #   name = "student_t",
    #   params = list(
    #     param1 = list(
    #       name = "df",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = NULL,
    #     param3 = NULL
    #     )
    #   ),

    # studentized_range = list(
    #   distribution = "ptukey",
    #   name = "studentized_range",
    #   params = list(
    #     param1 = list(
    #       name = "nmeans",
    #       bounds = c(0, Inf),
    #       isInt = TRUE
    #       ),
    #     param2 = list(
    #       name = "df",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    triangular = list(
      distribution = "triangle",
      name = "triangular",
      params = list(
        param1 = list(
          name = "a",
          bounds = c(-Inf, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "b",
          bounds = c(-Inf, Inf), # actuallly c(a, Inf)
          isInt = FALSE
          ),
        param3 = list(
          name = "c",
          bounds = c(-Inf, Inf), # actually, c(a, b)
          isInt = FALSE
          )
        ),
      bounds = c(-Inf, Inf),
      discrete = FALSE
      ),

    uniform = list(
      distribution = "unif",
      name = "uniform",
      params = list(
        param1 = list(
          name = "min",
          bounds = c(-Inf, Inf),
          isInt = FALSE
          ),
        param2 = list(
          name = "max",
          bounds = c(-Inf, Inf), # actually, c(min, Inf)
          isInt = FALSE
          ),
        param3 = NULL
        ),
      bounds = c(-Inf, Inf),
      discrete = FALSE
      )

    # weibull = list(
    #   distribution = "weibull",
    #   name = "weibull",
    #   params = list(
    #     param1 = list(
    #       name = "shape",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param2 = list(
    #       name = "scale",
    #       bounds = c(0, Inf),
    #       isInt = FALSE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    # wilcoxon_rank_sum_statistic = list(
    #   distribution = "wilcox",
    #   name = "wilcoxon_rank_sum_statistic",
    #   params = list(
    #     param1 = list(
    #       name = "m",
    #       bounds = c(0, Inf),
    #       isInt = TRUE
    #       ),
    #     param2 = list(
    #       name = n,
    #       bounds = c(0, Inf),
    #       isInt = TRUE
    #       ),
    #     param3 = NULL
    #     )
    #   ),

    # wilcoxon_signed_rank_statistic = list(
    #   distribution = "signrank",
    #   name = "wilcoxon_signed_rank_statistic",
    #   params = list(
    #     param1 = list(
    #       name = "n",
    #       bounds = c(0, Inf),
    #       isInt = TRUE
    #       ),
    #     param2 = NULL,
    #     param3 = NULL
    #     )
    #   )
  )
}
