# Simulated stand-in for the thesis data, shared by the explanatory examples.
#
# The thesis used Growing Up in Ireland data, which is licensed and is not in
# this repository. These examples simulate students with the same structure:
#
#   - the same 8 'true' predictors, with the same factor levels (18 indicators);
#   - the coefficients published in the thesis (section 1.3) as the true model;
#   - 6 'noise' predictors, three related to true predictors (multicollinearity)
#     and three unrelated to anything;
#   - the response: whether a secondary-school student finds maths
#     'Not interesting' (1) or not (0).
#
# How often each level occurs is an assumption made here; the thesis does not
# report it. Variable names are descriptive, not the survey's codes.

THESIS_COEFFICIENTS <- c(
  "(Intercept)"                    = -0.6665,
  "transition_difficulty(13,15]"   =  0.18551,
  "transition_difficulty(15,35]"   =  0.22080,
  "transition_difficulty[5,10]"    = -0.0051,
  "like_school_primaryNever"       =  0.40353,
  "like_school_primarySometimes"   =  0.03793,
  "like_maths_primaryNever"        =  1.04975,
  "like_maths_primarySometimes"    =  0.46973,
  "teacher_positive>3"             = -0.4652,
  "teacher_positive2.75"           = -0.3579,
  "teacher_positive3"              = -0.2925,
  "teacher_negative1"              = -1.0452,
  "teacher_negative1.5"            = -0.6855,
  "teacher_negative2"              = -0.3858,
  "school_yearSecond year"         = -0.3800,
  "genderMale"                     = -0.2400,
  "qualification_soughtDegree"     = -0.2434,
  "qualification_soughtJunior Cert"  =  0.05743,
  "qualification_soughtLeaving Cert" = -0.0528
)

TRUE_PREDICTORS <- c("transition_difficulty", "like_school_primary", "like_maths_primary",
                     "teacher_positive", "teacher_negative", "school_year", "gender",
                     "qualification_sought")
NOISE_PREDICTORS <- c("carer_qualification", "social_class", "parent_teacher_meeting",
                      "induction_day", "religious_ethos", "teacher_adequacy")

pick <- function(n, levels, prob) factor(sample(levels, n, TRUE, prob), levels = levels)

# A noise variable that copies a true predictor some of the time (multicollinearity).
related <- function(source, levels, share) {
  own <- sample(levels, length(source), TRUE)
  copied <- levels[(as.integer(source) - 1) %% length(levels) + 1]
  factor(ifelse(runif(length(source)) < share, copied, own), levels = levels)
}

simulate_students <- function(n, noise_sd = 0) {
  d <- data.frame(
    transition_difficulty = pick(n, c("(10,13]", "(13,15]", "(15,35]", "[5,10]"), c(.25, .25, .25, .25)),
    like_school_primary   = pick(n, c("Always", "Never", "Sometimes"), c(.35, .15, .50)),
    like_maths_primary    = pick(n, c("Always", "Never", "Sometimes"), c(.35, .20, .45)),
    teacher_positive      = pick(n, c("<=2.5", ">3", "2.75", "3"), c(.25, .25, .25, .25)),
    teacher_negative      = pick(n, c("<1", "1", "1.5", "2"), c(.25, .25, .25, .25)),
    school_year           = pick(n, c("First year", "Second year"), c(.5, .5)),
    gender                = pick(n, c("Female", "Male"), c(.5, .5)),
    qualification_sought  = pick(n, c("Certificate", "Degree", "Junior Cert", "Leaving Cert"),
                                 c(.15, .55, .05, .25))
  )
  d$carer_qualification    <- related(d$qualification_sought, c("Lower", "Middle", "Higher"), 0.6)
  d$social_class           <- related(d$qualification_sought, c("A", "B", "C"), 0.5)
  d$parent_teacher_meeting <- related(d$teacher_positive, c("Yes", "No"), 0.5)
  d$induction_day          <- pick(n, c("Yes", "No"), c(.7, .3))
  d$religious_ethos        <- pick(n, c("Catholic", "Other", "None"), c(.6, .2, .2))
  d$teacher_adequacy       <- pick(n, c("Yes", "No"), c(.8, .2))

  X <- model.matrix(reformulate(TRUE_PREDICTORS), d)
  eta <- drop(X[, names(THESIS_COEFFICIENTS)] %*% THESIS_COEFFICIENTS)
  if (noise_sd > 0) eta <- eta + rnorm(n, 0, noise_sd)   # scenario 3: random 'noise' in the linear predictor
  d$not_interested <- rbinom(n, 1, plogis(eta))
  d
}

# Map selected indicator columns (e.g. "teacher_negative1.5") back to predictor names,
# as the thesis code did: a predictor counts as selected if any of its levels is.
predictors_from_columns <- function(columns, candidates) {
  candidates[vapply(candidates, function(p) any(startsWith(columns, p)), logical(1))]
}

output_path <- function(name) {
  dir.create("output", showWarnings = FALSE)
  file.path("output", name)
}
