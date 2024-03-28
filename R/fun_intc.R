# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# pkg <- c(
#   'atlas.eqvl' #Equivalence metric
# )
#
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
#
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})

# [FUNCTIONS] ------------------------------------------
# - Educational equivalence function -------------------------------
fun_intc_equivalence_education <- function(
    dbl_years_education
    , dbl_years_education_min
){

  # Arguments validation
  stopifnot(
    "'dbl_years_education' must be numeric." =
      is.numeric(dbl_years_education)
  )

  stopifnot(
    "'dbl_years_education_min' must be numeric." =
      is.numeric(dbl_years_education_min)
  )

  # Data wrangling
  rep(
    dbl_years_education
    , each = length(
      dbl_years_education_min
    )) -> dbl_years_education

  # Apply equivalence function to years of education
  fun_eqvl_equivalence(
    dbl_var = dbl_years_education
    , dbl_scale_ub = dbl_years_education_min
    , dbl_scaling = dbl_years_education_min
  ) -> dbl_eq_education

  rm(dbl_years_education)
  rm(dbl_years_education_min)

  # Truncate educational equivalence to [0,1]
  pmin(dbl_eq_education, 1) -> dbl_eq_education
  pmax(dbl_eq_education, 0) -> dbl_eq_education

  # Output
  return(dbl_eq_education)

}

# - Interchangeability (ß) function ---------------------------------------
fun_intc_ss <- function(
    dbl_similarity,
    dbl_competence,
    dbl_years = NULL,
    dbl_years_min = NULL
){
  
  # arguments validation
  stopifnot(
    "'dbl_similarity' must be a numeric vector of similarity scores between 0 and 1." = 
      any(
        is.na(dbl_similarity),
        all(
          is.numeric(dbl_similarity)
          , dbl_similarity >= 0
          , dbl_similarity <= 1
        )
      )
  )
  
  stopifnot(
    "'dbl_years' must be either NULL or a non-negative numeric vector of years of education and experience." = 
      any(
        is.null(dbl_years),
        is.na(dbl_years),
        all(
          is.numeric(dbl_years),
          dbl_years >= 0
        )
      )
  )
  
  stopifnot(
    "'dbl_years_min' must be either NULL or a non-negative numeric vector of required years of education and experience." = 
      any(
        is.null(dbl_years_min),
        is.na(dbl_years_min),
        all(
          is.numeric(dbl_years_min),
          dbl_years_min >= 0
        )
      )
  )
  
  stopifnot(
    "'dbl_competence' must be a numeric vector of competence scores between 0 and 1." = 
      any(
        is.na(dbl_competence),
        all(
          is.numeric(dbl_competence)
          , dbl_competence >= 0
          , dbl_competence <= 1
        )
      )
  )
  
  # tau (education + xp) equivalence
  # default value if years are NULL 
  dbl_years_eqvl -> 1
    
  if(all(
    length(dbl_years),
    length(dbl_years_min)
  )){
    
    fun_eqvl_years(
      dbl_years = dbl_years,
      dbl_years_min = dbl_years_min,
      dbl_scaling = dbl_competence
    ) -> dbl_years_eqvl
    
  }
  
  rm(dbl_years)
  rm(dbl_years_min)
  
  # similarity equivalence
  fun_eqvl_similarity(
    dbl_similarity = dbl_similarity,
    dbl_midpoint = dbl_competence
  ) -> dbl_similarity_eqvl
  
  rm(dbl_similarity)
  rm(dbl_competence)
  
  # interchangeability
  dbl_similarity_eqvl * 
    dbl_years_eqvl ->
    dbl_interchangeability
  
  # output
  return(dbl_interchangeability)
  
}

# - Hireability binary indicator function ---------------------------------
# fun_intc_hireability <- function(dbl_interchangeability, dbl_competence?)
# h = as.numeric(\ss >= 0.5?)
# h = as.numeric(\ss >= c?)
# h = as.numeric(\ss >= 0.5 ^ (1 - c)?)

# # - Interchangeability function -------------------------------------------
# fun_intc_interchangeability <- function(
#     dbl_similarity
#     , dbl_scaling = 1
#     , dbl_years_education = NULL
#     , dbl_years_education_min = NULL
# ){
# 
#   # Other arguments validation within helper functions
#   stopifnot(
#     "'dbl_similarity' must be a percentage." =
#       all(
#         dbl_similarity >= 0,
#         dbl_similarity <= 1
#       )
#   )
# 
#   # Data wrangling
#   dbl_scaling[[1]] -> dbl_scaling
# 
#   # Apply equivalence function to similarity scores
#   fun_eqvl_equivalence(
#     dbl_var = dbl_similarity
#     , dbl_scaling = dbl_scaling
#   ) -> dbl_interchangeability
# 
#   rm(dbl_similarity)
# 
#   # Apply equivalence function to years of education
#   if(all(
#     length(dbl_years_education),
#     length(dbl_years_education_min)
#   )){
# 
#     fun_intc_equivalence_education(
#       dbl_years_education =
#         dbl_years_education
#       , dbl_years_education_min =
#         dbl_years_education_min
#     ) *
#       dbl_interchangeability ->
#       dbl_interchangeability
# 
#   }
# 
#   rm(dbl_years_education)
#   rm(dbl_years_education_min)
# 
#   # Apply equivalence function to Atlas Career Type
#   # fun_eqvl_equivalence_acti
# 
#   # Data wrangling
# 
#   # Output
#   return(dbl_interchangeability)
# 
# }

# - Interchangeability function -------------------------------------------
fun_intc_interchangeability <- function(
    dbl_similarity
    , dbl_scaling = 1
    , dbl_years_education = NULL
    , dbl_years_education_min = NULL
){

  # Other arguments validation within helper functions
  stopifnot(
    "'dbl_similarity' must be a percentage." =
      all(
        dbl_similarity >= 0,
        dbl_similarity <= 1
      )
  )

  # Data wrangling
  dbl_scaling[[1]] -> dbl_scaling

  # Apply equivalence function to similarity scores
  fun_eqvl_equivalence(
    dbl_var = dbl_similarity
    , dbl_scaling = dbl_scaling
  ) -> dbl_interchangeability

  rm(dbl_similarity)

  # Apply equivalence function to years of education
  if(all(
    length(dbl_years_education),
    length(dbl_years_education_min)
  )){

    fun_intc_equivalence_education(
      dbl_years_education =
        dbl_years_education
      , dbl_years_education_min =
        dbl_years_education_min
    ) *
      dbl_interchangeability ->
      dbl_interchangeability

  }

  rm(dbl_years_education)
  rm(dbl_years_education_min)

  # Apply equivalence function to Atlas Career Type
  # fun_eqvl_equivalence_acti

  # Data wrangling

  # Output
  return(dbl_interchangeability)

}

# # [TEST] ------------------------------------------------------------------
# # - Interchangeability test 1 -----------------------------------------------
# fun_intc_interchangeability(
#   dbl_similarity = runif(1, 0, 1)
#   , dbl_scaling = 1
# )
# 
# # - Interchangeability test 2 -----------------------------------------------
# fun_intc_interchangeability(
#   dbl_similarity = runif(1, 0, 1)
#   , dbl_scaling = 1
#   , dbl_years_education = 21
#   , dbl_years_education_min = 25
# )
