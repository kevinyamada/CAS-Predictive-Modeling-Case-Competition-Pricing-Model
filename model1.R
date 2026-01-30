library(readxl)
library(dplyr)

DATA_PATH <- "C:/Users/kevin/Downloads/CAS Case competition/06 - CAS Predictive Modeling Case Competition- Dataset.xlsx"
K_FOLDS <- 5
set.seed(1)

SEV_CAP_P <- 0.95
GM_SEV_CAP_P <- 0.85
LIAB_SEV_CAP_P <- 0.97
MIN_POS_FOR_CAP <- 30

UNCAP_ALPHA <- 0.20
GM_UNCAP_ALPHA <- 0.00
LIAB_UNCAP_ALPHA <- 0.95

PRED_CAP_P <- 0.97
GM_PRED_CAP_P <- 0.92
LIAB_PRED_CAP_P <- 0.995
MIN_N_FOR_PRED_CAP <- 50

LIAB_BLEND_W <- 0.40

CAL_CLAIM_K <- 1000
CAL_CAP_LOW <- 0.70
CAL_CAP_HIGH <- 1.30

fixed_exp_renew <- 25
var_exp_renew   <- 0.30
fixed_exp_new   <- 35
var_exp_new     <- 0.35
profit          <- 0.05

alpha    <- 0.35
cap_low  <- 0.70
cap_high <- 1.50

USE_CRED_TIER_SMOOTH <- TRUE
CRED_K <- 800

clean_cat <- function(x) factor(ifelse(is.na(x), "Unknown", as.character(x)))

make_bands <- function(df) {
  df %>%
    mutate(
      gpa_band = case_when(
        is.na(gpa) ~ "Unknown",
        gpa < 2.0  ~ "<2.0",
        gpa < 3.0  ~ "2.0-2.99",
        TRUE       ~ "3.0+"
      ) %>% factor(),
      dist_band = case_when(
        is.na(distance_to_campus) ~ "Unknown",
        distance_to_campus == 0   ~ "0",
        distance_to_campus <= 1   ~ "0-1",
        distance_to_campus <= 3   ~ "1-3",
        TRUE                      ~ "3+"
      ) %>% factor()
    )
}

align_to_train_levels <- function(df, train_ref) {
  df %>%
    mutate(
      coverage    = factor(ifelse(is.na(coverage), "Unknown", as.character(coverage)), levels = levels(train_ref$coverage)),
      risk_tier   = factor(ifelse(is.na(risk_tier), "Unknown", as.character(risk_tier)), levels = levels(train_ref$risk_tier)),
      greek       = factor(ifelse(is.na(greek), "Unknown", as.character(greek)), levels = levels(train_ref$greek)),
      off_campus  = factor(ifelse(is.na(off_campus), "Unknown", as.character(off_campus)), levels = levels(train_ref$off_campus)),
      sprinklered = factor(ifelse(is.na(sprinklered), "Unknown", as.character(sprinklered)), levels = levels(train_ref$sprinklered)),
      gender      = factor(ifelse(is.na(gender), "Unknown", as.character(gender)), levels = levels(train_ref$gender)),
      gpa_band    = factor(as.character(gpa_band),  levels = levels(train_ref$gpa_band)),
      dist_band   = factor(as.character(dist_band), levels = levels(train_ref$dist_band))
    )
}

gross_premium <- function(loss_cost, fixed_exp, var_exp, profit) {
  (loss_cost + fixed_exp) / (1 - var_exp - profit)
}

FREQ_FORMULA <- has_claim ~ risk_tier + off_campus + sprinklered + greek + dist_band + gpa_band
SEV_FORMULA  <- total_paid_cap ~ risk_tier + off_campus + sprinklered + greek + dist_band + gpa_band

fit_2part_for_coverage <- function(cov_name, df_train) {
  dfc <- df_train %>% filter(coverage == cov_name) %>% droplevels()
  dfc <- dfc %>% mutate(has_claim = total_paid > 0)
  
  pos <- dfc %>% filter(total_paid > 0)
  
  cap_p <- if (cov_name == "Guest Medical") GM_SEV_CAP_P else if (cov_name == "Liability") LIAB_SEV_CAP_P else SEV_CAP_P
  cap <- if (nrow(pos) >= MIN_POS_FOR_CAP) as.numeric(quantile(pos$total_paid, cap_p, na.rm = TRUE)) else Inf
  
  dfc <- dfc %>% mutate(total_paid_cap = ifelse(total_paid > 0, pmin(total_paid, cap), 0))
  
  uncap_full <- if (is.finite(cap) && nrow(pos) >= MIN_POS_FOR_CAP) {
    mean(pos$total_paid, na.rm = TRUE) / mean(pmin(pos$total_paid, cap), na.rm = TRUE)
  } else 1.0
  
  uncap_pow <- if (cov_name == "Guest Medical") GM_UNCAP_ALPHA else if (cov_name == "Liability") LIAB_UNCAP_ALPHA else UNCAP_ALPHA
  uncap_adj <- uncap_full^uncap_pow
  
  n_claim <- sum(dfc$has_claim, na.rm = TRUE)
  freq_formula <- if (n_claim < 20) has_claim ~ 1 else FREQ_FORMULA
  freq <- suppressWarnings(glm(freq_formula, data = dfc, family = binomial(), control = glm.control(maxit = 200)))
  
  pos2 <- dfc %>% filter(total_paid_cap > 0)
  sev_formula <- if (nrow(pos2) < 30) total_paid_cap ~ 1 else SEV_FORMULA
  sev <- suppressWarnings(glm(sev_formula, data = pos2, family = Gamma(link = "log"), control = glm.control(maxit = 200)))
  
  list(freq = freq, sev = sev, uncap_adj = uncap_adj)
}

fit_all_coverages <- function(df_train, coverages) {
  fits <- lapply(coverages, fit_2part_for_coverage, df_train = df_train)
  names(fits) <- coverages
  fits
}

predict_pure_by_coverage <- function(fits, df_new) {
  out <- rep(NA_real_, nrow(df_new))
  covs_present <- unique(as.character(df_new$coverage))
  
  for (cov in covs_present) {
    idx <- which(as.character(df_new$coverage) == cov)
    f <- fits[[cov]]
    if (is.null(f)) next
    
    p <- suppressWarnings(predict(f$freq, newdata = df_new[idx, ], type = "response"))
    s <- suppressWarnings(predict(f$sev,  newdata = df_new[idx, ], type = "response"))
    out[idx] <- p * s * f$uncap_adj
  }
  out
}

raw <- read_excel(DATA_PATH)

data_cov <- raw %>%
  group_by(student_id, coverage) %>%
  summarise(
    risk_tier = first(risk_tier),
    gpa = first(gpa),
    greek = first(greek),
    off_campus = first(off_campus),
    distance_to_campus = first(distance_to_campus),
    sprinklered = first(sprinklered),
    gender = first(gender),
    holdout = first(holdout),
    total_paid = sum(amount, na.rm = TRUE),
    .groups = "drop"
  )

train <- data_cov %>% filter(holdout == FALSE)
test  <- data_cov %>% filter(holdout == TRUE)

train2 <- train %>%
  mutate(
    coverage    = clean_cat(coverage),
    risk_tier   = clean_cat(risk_tier),
    greek       = clean_cat(greek),
    off_campus  = clean_cat(off_campus),
    sprinklered = clean_cat(sprinklered),
    gender      = clean_cat(gender)
  ) %>%
  make_bands()

test2 <- test %>%
  mutate(
    coverage    = clean_cat(coverage),
    risk_tier   = clean_cat(risk_tier),
    greek       = clean_cat(greek),
    off_campus  = clean_cat(off_campus),
    sprinklered = clean_cat(sprinklered),
    gender      = clean_cat(gender)
  ) %>%
  make_bands() %>%
  align_to_train_levels(train2)

coverages <- levels(train2$coverage)

train2_cv <- train2 %>%
  group_by(coverage) %>%
  mutate(fold = sample(rep(1:K_FOLDS, length.out = n()))) %>%
  ungroup()

oof_pred <- rep(NA_real_, nrow(train2_cv))

for (k in 1:K_FOLDS) {
  train_k <- train2_cv %>% filter(fold != k) %>% align_to_train_levels(train2)
  val_k   <- train2_cv %>% filter(fold == k) %>% align_to_train_levels(train2)
  
  fits_k <- fit_all_coverages(train_k, coverages)
  oof_pred[train2_cv$fold == k] <- predict_pure_by_coverage(fits_k, val_k)
}

train2_cv$oof_pred <- oof_pred

cal_oof <- train2_cv %>%
  group_by(coverage) %>%
  summarise(
    n_claim = sum(total_paid > 0, na.rm = TRUE),
    y = sum(total_paid, na.rm = TRUE),
    p = sum(oof_pred,  na.rm = TRUE),
    raw_cal = y / pmax(p, 1e-9),
    w = n_claim / (n_claim + CAL_CLAIM_K),
    cal_factor = 1 + (raw_cal - 1) * w,
    cal_factor = pmin(pmax(cal_factor, CAL_CAP_LOW), CAL_CAP_HIGH),
    .groups = "drop"
  )

fits_final <- fit_all_coverages(train2, coverages)

pred_train <- train2 %>%
  mutate(pred_loss = predict_pure_by_coverage(fits_final, train2)) %>%
  left_join(cal_oof %>% select(coverage, cal_factor), by = "coverage") %>%
  mutate(pred_loss_cal = pred_loss * cal_factor)

pred_test <- test2 %>%
  mutate(pred_loss = predict_pure_by_coverage(fits_final, test2)) %>%
  left_join(cal_oof %>% select(coverage, cal_factor), by = "coverage") %>%
  mutate(pred_loss_cal = pred_loss * cal_factor)

pred_cap_by_cov <- pred_train %>%
  group_by(coverage) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    p_cap = case_when(
      as.character(coverage) == "Guest Medical" ~ GM_PRED_CAP_P,
      as.character(coverage) == "Liability"     ~ LIAB_PRED_CAP_P,
      TRUE                                      ~ PRED_CAP_P
    ),
    pred_cap = case_when(
      n < MIN_N_FOR_PRED_CAP ~ Inf,
      TRUE ~ NA_real_
    )
  )

pred_cap_by_cov <- pred_cap_by_cov %>%
  rowwise() %>%
  mutate(
    pred_cap = if (is.finite(pred_cap)) pred_cap else as.numeric(quantile(
      pred_train$pred_loss_cal[pred_train$coverage == coverage],
      p_cap, na.rm = TRUE
    ))
  ) %>%
  ungroup() %>%
  select(coverage, pred_cap)

pred_train <- pred_train %>%
  select(-any_of(c("pred_cap","pred_loss_cap","tier_mean_cap","pred_loss_final"))) %>%
  left_join(pred_cap_by_cov, by = "coverage") %>%
  mutate(pred_loss_cap = pmin(pred_loss_cal, pred_cap))

pred_test <- pred_test %>%
  select(-any_of(c("pred_cap","pred_loss_cap","tier_mean_cap","pred_loss_final"))) %>%
  left_join(pred_cap_by_cov, by = "coverage") %>%
  mutate(pred_loss_cap = pmin(pred_loss_cal, pred_cap))

tier_means_cap <- pred_train %>%
  group_by(coverage, risk_tier) %>%
  summarise(tier_mean_cap = mean(pred_loss_cap, na.rm = TRUE), .groups = "drop")

pred_train <- pred_train %>%
  select(-any_of(c("tier_mean_cap","tier_mean_cap.x","tier_mean_cap.y"))) %>%
  left_join(tier_means_cap, by = c("coverage", "risk_tier")) %>%
  mutate(
    pred_loss_final = ifelse(
      as.character(coverage) == "Liability",
      LIAB_BLEND_W * tier_mean_cap + (1 - LIAB_BLEND_W) * pred_loss_cap,
      pred_loss_cap
    )
  )

pred_test <- pred_test %>%
  select(-any_of(c("tier_mean_cap","tier_mean_cap.x","tier_mean_cap.y"))) %>%
  left_join(tier_means_cap, by = c("coverage", "risk_tier")) %>%
  mutate(
    pred_loss_final = ifelse(
      as.character(coverage) == "Liability",
      LIAB_BLEND_W * tier_mean_cap + (1 - LIAB_BLEND_W) * pred_loss_cap,
      pred_loss_cap
    )
  )

holdout_check <- pred_test %>%
  group_by(coverage) %>%
  summarise(
    actual = mean(total_paid),
    pred_c = mean(pred_loss_final),
    ratio  = pred_c / actual,
    .groups="drop"
  )

print(cal_oof)
print(holdout_check)

base_raw <- pred_train %>%
  group_by(coverage, risk_tier) %>%
  summarise(n = n(), base_loss = mean(pred_loss_final), .groups="drop")

if (USE_CRED_TIER_SMOOTH) {
  cov_overall <- pred_train %>%
    group_by(coverage) %>%
    summarise(cov_mean = mean(pred_loss_final), .groups="drop")
  
  base_loss_by_tier <- base_raw %>%
    left_join(cov_overall, by="coverage") %>%
    mutate(
      w = n / (n + CRED_K),
      base_loss = w * base_loss + (1 - w) * cov_mean
    ) %>%
    select(coverage, risk_tier, base_loss)
} else {
  base_loss_by_tier <- base_raw %>% select(coverage, risk_tier, base_loss)
}

pricing_table <- base_loss_by_tier %>%
  mutate(
    prem_renew = gross_premium(base_loss, fixed_exp_renew, var_exp_renew, profit),
    prem_new   = gross_premium(base_loss, fixed_exp_new,   var_exp_new,   profit)
  )

tier_avg <- pred_train %>%
  group_by(coverage, risk_tier) %>%
  summarise(tier_pred_avg = mean(pred_loss_final), .groups="drop")

pricing_base <- pricing_table %>%
  select(coverage, risk_tier, prem_renew, prem_new)

final_train_prices <- pred_train %>%
  left_join(tier_avg, by=c("coverage","risk_tier")) %>%
  left_join(pricing_base, by=c("coverage","risk_tier")) %>%
  mutate(
    relativity_raw    = pred_loss_final / tier_pred_avg,
    relativity_shrunk = relativity_raw^alpha,
    relativity_cap    = pmin(pmax(relativity_shrunk, cap_low), cap_high),
    final_prem_renew  = prem_renew * relativity_cap,
    final_prem_new    = prem_new   * relativity_cap
  )

cap_stats <- final_train_prices %>%
  summarise(
    pct_capped_low  = mean(relativity_shrunk < cap_low,  na.rm=TRUE),
    pct_capped_high = mean(relativity_shrunk > cap_high, na.rm=TRUE),
    avg_relativity  = mean(relativity_cap, na.rm=TRUE)
  )

print(pricing_table)
print(cap_stats)

student_totals <- final_train_prices %>%
  group_by(student_id, risk_tier) %>%
  summarise(
    total_renew = sum(final_prem_renew, na.rm=TRUE),
    total_new   = sum(final_prem_new,   na.rm=TRUE),
    .groups="drop"
  )

print(student_totals %>%
        group_by(risk_tier) %>%
        summarise(
          avg_total_renew = mean(total_renew),
          avg_total_new   = mean(total_new),
          .groups="drop"
        ))

stopifnot(!any(is.na(student_totals$total_renew)))
stopifnot(!any(is.na(student_totals$total_new)))

holdout_check
cap_stats

write.csv(student_totals, "student_totals.csv", row.names = FALSE)
