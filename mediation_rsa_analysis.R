###############################################################################
# 中介型响应面分析 (Mediated RSA) — 基于 Fu, Dimotakis, & Koopman (2025) 的
# "disaggregated approach"（分解法）
#
# 研究设计:
#   X (家长报告教养) + Y (学生报告教养) → M (T3 中介变量) → Z (T4 学业倦怠)
#
# 核心预测变量组合 (与纵向 RSA 代码保持一致: X=家长, Y=学生):
#   (1) 过度养育: X = parent_overparenting_T2, Y = student_overparenting_T2
#   (2) 自主支持: X = parent_autonomy_support_T2, Y = student_autonomy_support_T2
#
# T3 中介变量 (7 个, 每个控制对应 T2 前测):
#   - academic_self_efficacy_T3  (学业自我效能感)
#   - intrinsic_value_T3         (内在价值)
#   - socioemotional_curiosity_T3 (社会情感好奇心, 社会情感能力量表子维度)
#   - interest_curiosity_T3      (兴趣型好奇心, 认知性好奇心子量表)
#   - deprivation_curiosity_T3   (剥夺型好奇心, 认知性好奇心子量表)
#   - cognitive_curiosity_T3     (认知性好奇心总分)
#   - grit_T3                    (坚毅)
#
# 结果变量 (T4):
#   - burnout_T4 (学业倦怠)
#
# 纵向中介设计控制:
#   - 每个中介模型控制相应中介变量的 T2 前测 (M_T2)
#   - 控制变量按模型路径区分:
#       a 路径 / b 路径: sex, SES, burnout_T2, M_T2
#       总效应:          sex, SES, burnout_T2
#   - 所有控制变量均值中心化 (修正绘图截距)
#
# 参考文献:
#   Fu, S. Q., Dimotakis, N., & Koopman, J. (2025). Mediation testing with
#     polynomial regression. Journal of Applied Psychology, 111(1), 1-17.
#   Edwards, J. R., & Parry, M. E. (1993). On the use of polynomial
#     regression equations as an alternative to difference scores in
#     organizational research. Academy of Management Journal, 36(6), 1577-1613.
###############################################################################

# =============================================================================
# 第 0 部分: 加载所需 R 包
# =============================================================================

required_packages <- c(
  "readxl", "dplyr", "tidyr", "car", "boot", "RSA", "lavaan",
  "ggplot2", "knitr", "kableExtra", "writexl"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

options(scipen = 999)
set.seed(2024)


# =============================================================================
# 第 1 部分: 读取数据与变量验证
# =============================================================================

# --- 1.1 读取 rsa_ready_plus_mediators (所有中介变量已在此 sheet 中) ---
data_path <- "matched_T2_T3_T4_with_T1sex_parent_caregiver_v4_plus_mediators.xlsx"
dat <- read_excel(data_path, sheet = "rsa_ready_plus_mediators")
cat("数据维度:", nrow(dat), "行 ×", ncol(dat), "列\n")
cat("变量名:\n")
print(names(dat))

# --- 1.2 量表范围验证 ---
SCALE_RANGES <- list(
  student_overparenting_T2    = c(1, 5),
  parent_overparenting_T2     = c(1, 5),
  student_autonomy_support_T2 = c(1, 7),
  parent_autonomy_support_T2  = c(1, 7),
  burnout_T2                  = c(1, 5),
  burnout_T4                  = c(1, 5),
  academic_self_efficacy_T2   = c(1, 7),
  academic_self_efficacy_T3   = c(1, 7),
  intrinsic_value_T2          = c(1, 7),
  intrinsic_value_T3          = c(1, 7),
  socioemotional_curiosity_T2 = c(1, 5),
  socioemotional_curiosity_T3 = c(1, 5),
  interest_curiosity_T2       = c(1, 5),
  interest_curiosity_T3       = c(1, 5),
  deprivation_curiosity_T2    = c(1, 5),
  deprivation_curiosity_T3    = c(1, 5),
  cognitive_curiosity_T2      = c(1, 5),
  cognitive_curiosity_T3      = c(1, 5),
  grit_T2                     = c(1, 5),
  grit_T3                     = c(1, 5)
)

check_scale_range <- function(data, ranges) {
  cat("\n--- 量表范围验证 ---\n")
  for (var_name in names(ranges)) {
    if (!var_name %in% names(data)) {
      cat("  [MISSING]", var_name, "\n")
      next
    }
    vals <- data[[var_name]]
    n_valid <- sum(!is.na(vals))
    if (n_valid == 0) {
      cat("  [WARNING]", var_name, ": 全部为 NA\n")
      next
    }
    actual_min <- min(vals, na.rm = TRUE)
    actual_max <- max(vals, na.rm = TRUE)
    rng <- ranges[[var_name]]
    if (actual_min < rng[1] - 0.01 || actual_max > rng[2] + 0.01) {
      cat("  [WARNING]", var_name, ": 超出预期范围 [", rng[1], ",", rng[2],
          "], 实际 [", round(actual_min, 2), ",", round(actual_max, 2), "]\n")
    } else {
      cat("  [OK]     ", var_name, ": [",
          round(actual_min, 2), ",", round(actual_max, 2),
          "] (n =", n_valid, ")\n")
    }
  }
}

check_scale_range(dat, SCALE_RANGES)

# --- 1.3 必需列检查 ---
REQUIRED_VARS <- c(
  "final_id", "sex_T1", "parent_SES_T2",
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  "burnout_T2", "burnout_T4",
  "has_T2_parent", "has_T3", "has_T4",
  "academic_self_efficacy_T2", "academic_self_efficacy_T3",
  "intrinsic_value_T2", "intrinsic_value_T3",
  "socioemotional_curiosity_T2", "socioemotional_curiosity_T3",
  "interest_curiosity_T2", "interest_curiosity_T3",
  "deprivation_curiosity_T2", "deprivation_curiosity_T3",
  "cognitive_curiosity_T2", "cognitive_curiosity_T3",
  "grit_T2", "grit_T3"
)

missing_vars <- setdiff(REQUIRED_VARS, names(dat))
if (length(missing_vars) > 0) {
  stop("缺少必需变量: ", paste(missing_vars, collapse = ", "))
}
cat("\n所有必需变量已确认存在。\n")

# --- 1.4 设计性筛选 ---
dat_analysis <- dat %>%
  filter(has_T2_parent == 1, has_T3 == 1, has_T4 == 1)

cat("设计性筛选后样本量 (has_T2_parent + has_T3 + has_T4):",
    nrow(dat_analysis), "\n")


# =============================================================================
# 第 2 部分: 变量中心化与多项式项
# =============================================================================

# --- 2.1 量表中点 (用于预测变量 midpoint centering) ---
OVERPARENTING_MIDPOINT    <- 3   # 过度养育 1-5 量表
AUTONOMY_SUPPORT_MIDPOINT <- 4   # 自主支持 1-7 量表

# --- 2.2 预测变量 midpoint centering ---
dat_analysis <- dat_analysis %>%
  mutate(
    X_op = parent_overparenting_T2  - OVERPARENTING_MIDPOINT,
    Y_op = student_overparenting_T2 - OVERPARENTING_MIDPOINT,
    X_as = parent_autonomy_support_T2  - AUTONOMY_SUPPORT_MIDPOINT,
    Y_as = student_autonomy_support_T2 - AUTONOMY_SUPPORT_MIDPOINT
  )

# --- 2.3 控制变量均值中心化 ---
dat_analysis <- dat_analysis %>%
  mutate(
    sex_T1_c = as.numeric(sex_T1) -
      mean(as.numeric(sex_T1), na.rm = TRUE),
    ses_control_c = as.numeric(parent_SES_T2) -
      mean(as.numeric(parent_SES_T2), na.rm = TRUE),
    baseline_burnout_c = as.numeric(burnout_T2) -
      mean(as.numeric(burnout_T2), na.rm = TRUE)
  )

# 中介变量 T2 前测均值中心化 (批量处理)
MEDIATOR_T2_VARS <- c(
  "academic_self_efficacy_T2",
  "intrinsic_value_T2",
  "socioemotional_curiosity_T2",
  "interest_curiosity_T2",
  "deprivation_curiosity_T2",
  "cognitive_curiosity_T2",
  "grit_T2"
)

for (v in MEDIATOR_T2_VARS) {
  v_c <- paste0(v, "_c")
  dat_analysis[[v_c]] <- as.numeric(dat_analysis[[v]]) -
    mean(as.numeric(dat_analysis[[v]]), na.rm = TRUE)
}

# --- 2.4 多项式项 ---
dat_analysis <- dat_analysis %>%
  mutate(
    X_op2     = X_op^2,
    X_op_Y_op = X_op * Y_op,
    Y_op2     = Y_op^2,
    X_as2     = X_as^2,
    X_as_Y_as = X_as * Y_as,
    Y_as2     = Y_as^2
  )

# --- 2.5 Pooled SD ---
SD_pooled_op <- sqrt(var(dat_analysis$X_op, na.rm = TRUE) +
                       var(dat_analysis$Y_op, na.rm = TRUE))
SD_pooled_as <- sqrt(var(dat_analysis$X_as, na.rm = TRUE) +
                       var(dat_analysis$Y_as, na.rm = TRUE))

cat("\n过度养育 pooled SD:", round(SD_pooled_op, 3), "\n")
cat("自主支持 pooled SD:", round(SD_pooled_as, 3), "\n")


# =============================================================================
# 第 3 部分: 核心分析函数
# =============================================================================

# --- 3.0 辅助: 验证控制变量 (自动丢弃全缺失或零变异) ---
validate_controls <- function(data, control_vars, label = "") {
  if (is.null(control_vars) || length(control_vars) == 0) return(character(0))
  good <- character(0)
  for (v in control_vars) {
    if (!v %in% names(data)) {
      warning(label, ": 控制变量 '", v, "' 不在数据中, 已丢弃。")
      next
    }
    vals <- data[[v]]
    if (all(is.na(vals))) {
      warning(label, ": 控制变量 '", v, "' 全为 NA, 已丢弃。")
      next
    }
    if (sd(vals, na.rm = TRUE) < 1e-10) {
      warning(label, ": 控制变量 '", v, "' 无变异, 已丢弃。")
      next
    }
    good <- c(good, v)
  }
  good
}

# --- 3.1 主分析函数 ---
run_mediated_rsa <- function(data,
                              x_var, y_var,
                              x2_var, xy_var, y2_var,
                              m_var,
                              z_var,
                              control_vars_a     = NULL,
                              control_vars_b     = NULL,
                              control_vars_total = NULL,
                              sd_pooled,
                              n_boot     = 5000,
                              conf_level = 0.95,
                              compute_bca = TRUE,
                              seed = 2024) {

  set.seed(seed)
  poly_vars <- c(x_var, y_var, x2_var, xy_var, y2_var)

  # ---- 模型特异性 listwise deletion ----
  all_vars <- unique(c(poly_vars, m_var, z_var,
                        control_vars_a, control_vars_b, control_vars_total))

  missing_cols <- setdiff(all_vars, names(data))
  if (length(missing_cols) > 0) {
    stop("变量不在数据中: ", paste(missing_cols, collapse = ", "))
  }

  dat_complete <- data %>%
    select(all_of(all_vars)) %>%
    na.omit()

  n <- nrow(dat_complete)
  cat("模型特异性完整案例样本量:", n, "\n")
  if (n < 30) warning("样本量不足 30, 结果可能不稳定。")

  # ---- 验证控制变量 ----
  control_vars_a     <- validate_controls(dat_complete, control_vars_a,     "a 路径")
  control_vars_b     <- validate_controls(dat_complete, control_vars_b,     "b/c' 路径")
  control_vars_total <- validate_controls(dat_complete, control_vars_total, "总效应")

  # ---- 构建公式 ----
  poly_terms <- paste(poly_vars, collapse = " + ")

  build_formula <- function(dv, pred_rhs, ctrls) {
    rhs <- pred_rhs
    if (length(ctrls) > 0) rhs <- paste(rhs, "+", paste(ctrls, collapse = " + "))
    as.formula(paste(dv, "~", rhs))
  }

  formula_a     <- build_formula(m_var, poly_terms, control_vars_a)
  formula_b     <- build_formula(z_var, paste(poly_terms, "+", m_var), control_vars_b)
  formula_total <- build_formula(z_var, poly_terms, control_vars_total)

  cat("a 路径公式:",     deparse(formula_a, width.cutoff = 200), "\n")
  cat("b/c' 路径公式:",  deparse(formula_b, width.cutoff = 200), "\n")
  cat("总效应公式:",     deparse(formula_total, width.cutoff = 200), "\n")

  # ---- 拟合 OLS 模型 ----
  model_a     <- lm(formula_a,     data = dat_complete)
  model_b     <- lm(formula_b,     data = dat_complete)
  model_total <- lm(formula_total, data = dat_complete)

  # ---- 提取多项式系数 ----
  coef_a <- coef(model_a);  coef_b <- coef(model_b);  coef_total <- coef(model_total)

  a1 <- coef_a[x_var];  a2 <- coef_a[y_var]
  a3 <- coef_a[x2_var]; a4 <- coef_a[xy_var]; a5 <- coef_a[y2_var]

  beta <- coef_b[m_var]

  c1 <- coef_b[x_var];  c2 <- coef_b[y_var]
  c3 <- coef_b[x2_var]; c4 <- coef_b[xy_var]; c5 <- coef_b[y2_var]

  t1 <- coef_total[x_var];  t2 <- coef_total[y_var]
  t3 <- coef_total[x2_var]; t4 <- coef_total[xy_var]; t5 <- coef_total[y2_var]

  # ---- 响应面特征 ----
  a_fit_slope    <- a1 + a2;       a_fit_curve    <- a3 + a4 + a5
  a_misfit_slope <- a1 - a2;       a_misfit_curve <- a3 - a4 + a5

  c_fit_slope    <- c1 + c2;       c_fit_curve    <- c3 + c4 + c5
  c_misfit_slope <- c1 - c2;       c_misfit_curve <- c3 - c4 + c5

  t_fit_slope    <- t1 + t2;       t_fit_curve    <- t3 + t4 + t5
  t_misfit_slope <- t1 - t2;       t_misfit_curve <- t3 - t4 + t5

  # ---- 瞬时效应 ----
  a_fit_high          <- a_fit_slope    + 2 * a_fit_curve    * sd_pooled
  a_fit_low           <- a_fit_slope    + 2 * a_fit_curve    * (-sd_pooled)
  a_misfit_excess     <- a_misfit_slope + 2 * a_misfit_curve * sd_pooled
  a_misfit_deficiency <- a_misfit_slope + 2 * a_misfit_curve * (-sd_pooled)

  # ---- 间接效应 ----
  ie_fit_slope          <- a_fit_slope    * beta
  ie_fit_curve          <- a_fit_curve    * beta
  ie_misfit_slope       <- a_misfit_slope * beta
  ie_misfit_curve       <- a_misfit_curve * beta
  ie_fit_high           <- a_fit_high          * beta
  ie_fit_low            <- a_fit_low           * beta
  ie_misfit_excess      <- a_misfit_excess     * beta
  ie_misfit_deficiency  <- a_misfit_deficiency * beta

  # ---- 主轴 ----
  compute_principal_axes <- function(b1, b2, b3, b4, b5) {
    denom_sp <- 4 * b3 * b5 - b4^2
    if (abs(denom_sp) < 1e-10)
      return(list(x0 = NA, y0 = NA, p11 = NA, p10 = NA, p21 = NA, p20 = NA))
    x0 <- (b2 * b4 - 2 * b1 * b5) / denom_sp
    y0 <- (b1 * b4 - 2 * b2 * b3) / denom_sp
    disc <- sqrt((b5 - b3)^2 + b4^2)
    if (abs(b4) < 1e-10) { p11 <- NA; p21 <- NA }
    else {
      p11 <- ((b5 - b3) + disc) / b4
      p21 <- ((b5 - b3) - disc) / b4
    }
    p10 <- if (!is.na(p11)) y0 - p11 * x0 else NA
    p20 <- if (!is.na(p21)) y0 - p21 * x0 else NA
    list(x0 = x0, y0 = y0, p11 = p11, p10 = p10, p21 = p21, p20 = p20)
  }

  pa_a     <- compute_principal_axes(a1, a2, a3, a4, a5)
  pa_total <- compute_principal_axes(t1, t2, t3, t4, t5)

  # ---- Bootstrap (含 tryCatch) ----
  N_PARAMS <- 36

  boot_func <- function(data, indices) {
    d <- data[indices, ]
    tryCatch({
      mod_a <- lm(formula_a, data = d)
      mod_b <- lm(formula_b, data = d)
      mod_t <- lm(formula_total, data = d)

      ca <- coef(mod_a); cb <- coef(mod_b); ct <- coef(mod_t)

      ba1 <- ca[x_var]; ba2 <- ca[y_var]; ba3 <- ca[x2_var]
      ba4 <- ca[xy_var]; ba5 <- ca[y2_var]
      bb  <- cb[m_var]
      bc1 <- cb[x_var]; bc2 <- cb[y_var]; bc3 <- cb[x2_var]
      bc4 <- cb[xy_var]; bc5 <- cb[y2_var]
      bt1 <- ct[x_var]; bt2 <- ct[y_var]; bt3 <- ct[x2_var]
      bt4 <- ct[xy_var]; bt5 <- ct[y2_var]

      a_fs <- ba1+ba2; a_fc <- ba3+ba4+ba5
      a_ms <- ba1-ba2; a_mc <- ba3-ba4+ba5
      a_fh <- a_fs + 2*a_fc*sd_pooled;  a_fl <- a_fs - 2*a_fc*sd_pooled
      a_me <- a_ms + 2*a_mc*sd_pooled;  a_md <- a_ms - 2*a_mc*sd_pooled

      c(ba1, ba2, ba3, ba4, ba5, bb,
        a_fs, a_fc, a_ms, a_mc,
        a_fh, a_fl, a_me, a_md,
        a_fs*bb, a_fc*bb, a_ms*bb, a_mc*bb,
        a_fh*bb, a_fl*bb, a_me*bb, a_md*bb,
        bc1+bc2, bc3+bc4+bc5, bc1-bc2, bc3-bc4+bc5,
        bt1+bt2, bt3+bt4+bt5, bt1-bt2, bt3-bt4+bt5,
        {pa <- compute_principal_axes(ba1, ba2, ba3, ba4, ba5);
         c(pa$x0, pa$y0, pa$p11, pa$p10, pa$p21, pa$p20)})
    }, error = function(e) rep(NA_real_, N_PARAMS))
  }

  cat("开始 Bootstrap (", n_boot, " 次) ...\n")
  boot_results <- boot(dat_complete, boot_func, R = n_boot)

  n_failed <- sum(apply(boot_results$t, 1, function(r) any(is.na(r))))
  if (n_failed > 0) cat("  注意:", n_failed, "/", n_boot, " 次迭代失败 (NA)。\n")
  cat("Bootstrap 完成。\n")

  # ---- 构建结果表 (Percentile + BCa) ----
  alpha_ci <- (1 - conf_level) / 2

  param_names <- c(
    "a1 (X->M)", "a2 (Y->M)", "a3 (X2->M)", "a4 (XY->M)", "a5 (Y2->M)",
    "beta (M->Z)",
    "a_fit_slope", "a_fit_curve", "a_misfit_slope", "a_misfit_curve",
    "a_fit_high (+1SD)", "a_fit_low (-1SD)",
    "a_misfit_excess (+1SD)", "a_misfit_deficiency (-1SD)",
    "IE_fit_slope", "IE_fit_curve", "IE_misfit_slope", "IE_misfit_curve",
    "IE_fit_high (+1SD)", "IE_fit_low (-1SD)",
    "IE_misfit_excess (+1SD)", "IE_misfit_deficiency (-1SD)",
    "c'_fit_slope", "c'_fit_curve", "c'_misfit_slope", "c'_misfit_curve",
    "total_fit_slope", "total_fit_curve", "total_misfit_slope", "total_misfit_curve",
    "PA_x0", "PA_y0", "PA_p11", "PA_p10", "PA_p21", "PA_p20"
  )

  ie_indices <- 15:22

  results_table <- data.frame(
    Parameter       = param_names,
    Estimate        = NA_real_,
    SE              = NA_real_,
    CI_Lower_Perc   = NA_real_,
    CI_Upper_Perc   = NA_real_,
    Sig_Perc        = NA_character_,
    CI_Lower_BCa    = NA_real_,
    CI_Upper_BCa    = NA_real_,
    Sig_BCa         = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(param_names)) {
    est  <- boot_results$t0[i]
    dist <- boot_results$t[, i]
    se   <- sd(dist, na.rm = TRUE)
    pci  <- quantile(dist, probs = c(alpha_ci, 1 - alpha_ci), na.rm = TRUE)

    results_table$Estimate[i]      <- round(est, 4)
    results_table$SE[i]            <- round(se, 4)
    results_table$CI_Lower_Perc[i] <- round(pci[1], 4)
    results_table$CI_Upper_Perc[i] <- round(pci[2], 4)
    results_table$Sig_Perc[i]      <- ifelse(pci[1] * pci[2] > 0, "*", "")

    if (compute_bca && i %in% ie_indices) {
      bca <- tryCatch({
        ci_obj <- boot.ci(boot_results, index = i, type = "bca", conf = conf_level)
        c(ci_obj$bca[4], ci_obj$bca[5])
      }, error = function(e) c(NA_real_, NA_real_))
      results_table$CI_Lower_BCa[i] <- round(bca[1], 4)
      results_table$CI_Upper_BCa[i] <- round(bca[2], 4)
      results_table$Sig_BCa[i] <- ifelse(
        !is.na(bca[1]) && !is.na(bca[2]) && bca[1] * bca[2] > 0, "*", "")
    }
  }

  # ---- 返回 ----
  list(
    model_a = model_a, model_b = model_b, model_total = model_total,
    formula_a = formula_a, formula_b = formula_b, formula_total = formula_total,
    coefs = list(
      a_path  = c(a1 = a1, a2 = a2, a3 = a3, a4 = a4, a5 = a5),
      beta    = beta,
      c_prime = c(c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5),
      total   = c(t1 = t1, t2 = t2, t3 = t3, t4 = t4, t5 = t5)
    ),
    surface_chars = list(
      a_path  = c(fit_slope = a_fit_slope, fit_curve = a_fit_curve,
                  misfit_slope = a_misfit_slope, misfit_curve = a_misfit_curve),
      c_prime = c(fit_slope = c_fit_slope, fit_curve = c_fit_curve,
                  misfit_slope = c_misfit_slope, misfit_curve = c_misfit_curve),
      total   = c(fit_slope = t_fit_slope, fit_curve = t_fit_curve,
                  misfit_slope = t_misfit_slope, misfit_curve = t_misfit_curve)
    ),
    instantaneous = list(
      fit_high = a_fit_high, fit_low = a_fit_low,
      misfit_excess = a_misfit_excess, misfit_deficiency = a_misfit_deficiency
    ),
    indirect_effects = list(
      ie_fit_slope = ie_fit_slope, ie_fit_curve = ie_fit_curve,
      ie_misfit_slope = ie_misfit_slope, ie_misfit_curve = ie_misfit_curve,
      ie_fit_high = ie_fit_high, ie_fit_low = ie_fit_low,
      ie_misfit_excess = ie_misfit_excess, ie_misfit_deficiency = ie_misfit_deficiency
    ),
    principal_axes = pa_a,
    principal_axes_total = pa_total,
    boot_results   = boot_results,
    results_table  = results_table,
    sd_pooled      = sd_pooled,
    n = n,
    n_boot_failed = n_failed
  )
}


# =============================================================================
# 第 4 部分: 响应面绘图函数
# =============================================================================

plot_response_surface <- function(coefs, title = "",
                                  xlim = c(-2, 2), ylim = c(-2, 2),
                                  zlim = NULL,
                                  xlab = "Parent Report (X)",
                                  ylab = "Student Report (Y)",
                                  zlab = "Outcome") {
  plotRSA(
    x  = coefs[1], y  = coefs[2],
    x2 = coefs[3], xy = coefs[4], y2 = coefs[5],
    b0 = if (length(coefs) >= 6) coefs[6] else 0,
    xlim = xlim, ylim = ylim,
    zlim = if (!is.null(zlim)) zlim else NULL,
    xlab = xlab, ylab = ylab, zlab = zlab,
    type = "3d", gridsize = 20, showSP = TRUE,
    axes = c("LOC", "LOIC", "PA1", "PA2"),
    axesStyles = list(
      LOC  = list(lty = "solid",  lwd = 1, col = "blue"),
      LOIC = list(lty = "solid",  lwd = 1, col = "red"),
      PA1  = list(lty = "dotted", lwd = 1, col = "darkgreen"),
      PA2  = list(lty = "dotted", lwd = 1, col = "purple")
    ),
    contour = list(show = TRUE, color = "grey80", highlight = c()),
    project = c("contour", "LOC", "LOIC", "PA1", "PA2"),
    main = title, legend = FALSE,
    cex.tickLabel = 0.7, cex.axesLabel = 0.7
  )
}

plot_indirect_surface <- function(a_coefs, beta, intercept_a = 0,
                                   title = "Indirect Effect Surface",
                                   xlim = c(-2, 2), ylim = c(-2, 2),
                                   xlab = "Parent Report (X)",
                                   ylab = "Student Report (Y)") {
  ie_coefs     <- a_coefs * beta
  ie_intercept <- intercept_a * beta
  plot_response_surface(
    coefs = c(ie_coefs, ie_intercept),
    title = title, xlim = xlim, ylim = ylim,
    xlab = xlab, ylab = ylab, zlab = "Indirect Effect"
  )
}

plot_line_effects <- function(coefs, sd_pooled,
                               title = "",
                               xlab_fit    = "Level of Congruence",
                               xlab_misfit = "Level of Incongruence",
                               ylab = "Predicted Outcome") {
  b1 <- coefs[1]; b2 <- coefs[2]; b3 <- coefs[3]
  b4 <- coefs[4]; b5 <- coefs[5]
  b0 <- if (length(coefs) >= 6) coefs[6] else 0

  x_seq <- seq(-2 * sd_pooled, 2 * sd_pooled, length.out = 100)

  fit_slope    <- b1 + b2;       fit_curve    <- b3 + b4 + b5
  misfit_slope <- b1 - b2;       misfit_curve <- b3 - b4 + b5

  y_fit    <- b0 + fit_slope    * x_seq + fit_curve    * x_seq^2
  y_misfit <- b0 + misfit_slope * x_seq + misfit_curve * x_seq^2

  par(mfrow = c(1, 2))

  plot(x_seq, y_fit, type = "l", lwd = 2, col = "blue",
       xlab = xlab_fit, ylab = ylab,
       main = paste(title, "- LOC (y = x)"))
  abline(v = 0, lty = 2, col = "grey50")
  abline(v = c(-sd_pooled, sd_pooled), lty = 3, col = "grey70")
  legend("topleft",
         legend = c(paste("Slope:", round(fit_slope, 3)),
                    paste("Curve:", round(fit_curve, 3))),
         bty = "n", cex = 0.8)

  plot(x_seq, y_misfit, type = "l", lwd = 2, col = "red",
       xlab = xlab_misfit, ylab = ylab,
       main = paste(title, "- LOIC (y = -x)"))
  abline(v = 0, lty = 2, col = "grey50")
  abline(v = c(-sd_pooled, sd_pooled), lty = 3, col = "grey70")
  text(-sd_pooled, max(y_misfit), "Deficiency\n(Y > X)", pos = 4, cex = 0.7)
  text( sd_pooled, max(y_misfit), "Excess\n(X > Y)",     pos = 2, cex = 0.7)
  legend("topleft",
         legend = c(paste("Slope:", round(misfit_slope, 3)),
                    paste("Curve:", round(misfit_curve, 3))),
         bty = "n", cex = 0.8)

  par(mfrow = c(1, 1))
}


# =============================================================================
# 第 5 部分: linearHypothesis 检验响应面特征
# =============================================================================

test_surface_characteristics <- function(model, x_var, y_var,
                                          x2_var, xy_var, y2_var) {
  tests <- list(
    fit_slope    = paste0(x_var, " + ", y_var, " = 0"),
    fit_curve    = paste0(x2_var, " + ", xy_var, " + ", y2_var, " = 0"),
    misfit_slope = paste0(x_var, " - ", y_var, " = 0"),
    misfit_curve = paste0(x2_var, " - ", xy_var, " + ", y2_var, " = 0")
  )

  results <- lapply(names(tests), function(name) {
    test  <- linearHypothesis(model, tests[[name]])
    f_val <- test$F[2]
    p_val <- test$`Pr(>F)`[2]
    coefs <- coef(model)
    est <- switch(name,
      fit_slope    = coefs[x_var] + coefs[y_var],
      fit_curve    = coefs[x2_var] + coefs[xy_var] + coefs[y2_var],
      misfit_slope = coefs[x_var] - coefs[y_var],
      misfit_curve = coefs[x2_var] - coefs[xy_var] + coefs[y2_var]
    )
    data.frame(Characteristic = name,
               Estimate = round(est, 4),
               F_value  = round(f_val, 3),
               p_value  = round(p_val, 4),
               Significant = ifelse(p_val < 0.05, "*", ""))
  })
  do.call(rbind, results)
}


# =============================================================================
# 第 6 部分: 结果输出函数
# =============================================================================

print_results <- function(result, model_label = "Model") {
  cat("\n")
  cat("=================================================================\n")
  cat("  中介 RSA 分析结果:", model_label, "\n")
  cat("=================================================================\n")
  cat("样本量:", result$n, "  |  Pooled SD:", round(result$sd_pooled, 3))
  if (result$n_boot_failed > 0)
    cat("  |  Bootstrap 失败:", result$n_boot_failed)
  cat("\n\n")

  cat("--- a 路径模型 (多项式 -> 中介变量) ---\n")
  cat("公式:", deparse(result$formula_a, width.cutoff = 200), "\n")
  cat("R2 =", round(summary(result$model_a)$r.squared, 4),
      " Adj.R2 =", round(summary(result$model_a)$adj.r.squared, 4), "\n")
  print(summary(result$model_a)$coefficients)
  cat("\n")

  cat("--- b/c' 路径模型 (多项式 + 中介 -> 结果变量) ---\n")
  cat("公式:", deparse(result$formula_b, width.cutoff = 200), "\n")
  cat("R2 =", round(summary(result$model_b)$r.squared, 4),
      " Adj.R2 =", round(summary(result$model_b)$adj.r.squared, 4), "\n")
  print(summary(result$model_b)$coefficients)
  cat("\n")

  cat("--- 总效应模型 (多项式 -> 结果变量, 不含中介) ---\n")
  cat("公式:", deparse(result$formula_total, width.cutoff = 200), "\n")
  cat("R2 =", round(summary(result$model_total)$r.squared, 4),
      " Adj.R2 =", round(summary(result$model_total)$adj.r.squared, 4), "\n")
  print(summary(result$model_total)$coefficients)
  cat("\n")

  cat("--- a 路径响应面特征 ---\n")
  cat("一致线斜率  (a1+a2):",       round(result$surface_chars$a_path["fit_slope"], 4), "\n")
  cat("一致线曲率  (a3+a4+a5):",    round(result$surface_chars$a_path["fit_curve"], 4), "\n")
  cat("不一致线斜率 (a1-a2):",      round(result$surface_chars$a_path["misfit_slope"], 4), "\n")
  cat("不一致线曲率 (a3-a4+a5):",   round(result$surface_chars$a_path["misfit_curve"], 4), "\n\n")

  cat("--- 间接效应 (Bootstrap", nrow(result$boot_results$t), " 次) ---\n")
  ie_rows <- grep("^IE_", result$results_table$Parameter)
  ie_print <- result$results_table[ie_rows,
    c("Parameter", "Estimate", "SE",
      "CI_Lower_Perc", "CI_Upper_Perc", "Sig_Perc",
      "CI_Lower_BCa", "CI_Upper_BCa", "Sig_BCa")]
  print(ie_print, row.names = FALSE)
  cat("\n注: * 表示 CI 不包含 0; Perc = percentile; BCa = bias-corrected accelerated\n\n")

  cat("--- 完整结果汇总表 ---\n")
  print(result$results_table, row.names = FALSE)
  cat("\n")
}

export_results <- function(result, filename, model_label = "Model") {
  sheets <- list()

  sheets[["Full_Results"]] <- result$results_table

  fmt <- function(model) {
    s <- as.data.frame(summary(model)$coefficients)
    s$Variable <- rownames(s)
    s[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  }
  sheets[["A_Path_Model"]]        <- fmt(result$model_a)
  sheets[["B_Cprime_Model"]]      <- fmt(result$model_b)
  sheets[["Total_Effect_Model"]]  <- fmt(result$model_total)

  sheets[["Model_Fit"]] <- data.frame(
    Model = c("a_path", "b_cprime", "total"),
    R_squared = c(summary(result$model_a)$r.squared,
                  summary(result$model_b)$r.squared,
                  summary(result$model_total)$r.squared),
    Adj_R_squared = c(summary(result$model_a)$adj.r.squared,
                      summary(result$model_b)$adj.r.squared,
                      summary(result$model_total)$adj.r.squared),
    N = result$n,
    N_boot_failed = result$n_boot_failed
  )

  sheets[["Formulas"]] <- data.frame(
    Path    = c("a", "b_cprime", "total"),
    Formula = c(deparse(result$formula_a, width.cutoff = 500),
                deparse(result$formula_b, width.cutoff = 500),
                deparse(result$formula_total, width.cutoff = 500))
  )

  write_xlsx(sheets, path = filename)
  cat("结果已导出到:", filename, "\n")
}


# =============================================================================
# 第 7 部分: Alpha 路径决策逻辑
# =============================================================================

suggest_alpha_paths <- function(result, x_var, y_var, x2_var, xy_var, y2_var) {

  cat("\n--- Alpha 路径选择建议 ---\n\n")

  surface_tests <- test_surface_characteristics(
    result$model_a, x_var, y_var, x2_var, xy_var, y2_var
  )
  cat("a 路径响应面特征检验:\n")
  print(surface_tests, row.names = FALSE)
  cat("\n")

  fs_sig <- surface_tests$p_value[surface_tests$Characteristic == "fit_slope"]    < 0.05
  fc_sig <- surface_tests$p_value[surface_tests$Characteristic == "fit_curve"]    < 0.05
  ms_sig <- surface_tests$p_value[surface_tests$Characteristic == "misfit_slope"] < 0.05
  mc_sig <- surface_tests$p_value[surface_tests$Characteristic == "misfit_curve"] < 0.05

  cat("一致线 (LOC):\n")
  if (fs_sig && !fc_sig) {
    cat("  -> 仅斜率显著: alpha = fit_slope; IE = fit_slope * beta\n")
  } else if (!fs_sig && fc_sig) {
    cat("  -> 仅曲率显著: alpha = fit_curve; IE = fit_curve * beta\n")
  } else if (fs_sig && fc_sig) {
    cat("  -> 斜率+曲率均显著: 报告 +/-1 SD 瞬时效应\n")
  } else {
    cat("  -> 均不显著: 一致线上无显著 alpha 路径\n")
  }

  cat("\n不一致线 (LOIC):\n")
  if (!ms_sig && mc_sig) {
    cat("  -> 仅曲率显著: alpha = misfit_curve; IE = misfit_curve * beta\n")
  } else if (ms_sig && !mc_sig) {
    cat("  -> 仅斜率显著: alpha = misfit_slope; IE = misfit_slope * beta\n")
  } else if (ms_sig && mc_sig) {
    cat("  -> 斜率+曲率均显著: 报告 +/-1 SD 瞬时效应\n")
  } else {
    cat("  -> 均不显著: 不一致线上无显著 alpha 路径\n")
  }
  cat("\n")
  invisible(surface_tests)
}


# =============================================================================
# 第 8 部分: 运行正式中介分析 (2 预测变量组合 × 7 中介变量 = 14 个模型)
# =============================================================================

# 基础控制变量 (均值中心化版本)
CTRL_BASE <- c("sex_T1_c", "ses_control_c", "baseline_burnout_c")

# --- 中介变量配置 ---
MEDIATOR_SPECS <- list(
  list(m_t3 = "academic_self_efficacy_T3",
       m_t2 = "academic_self_efficacy_T2",
       label_en = "Academic Self-Efficacy",
       label_cn = "学业自我效能感"),
  list(m_t3 = "intrinsic_value_T3",
       m_t2 = "intrinsic_value_T2",
       label_en = "Intrinsic Value",
       label_cn = "内在价值"),
  list(m_t3 = "socioemotional_curiosity_T3",
       m_t2 = "socioemotional_curiosity_T2",
       label_en = "Socioemotional Curiosity",
       label_cn = "社会情感好奇心"),
  list(m_t3 = "interest_curiosity_T3",
       m_t2 = "interest_curiosity_T2",
       label_en = "Interest Curiosity",
       label_cn = "兴趣型好奇心"),
  list(m_t3 = "deprivation_curiosity_T3",
       m_t2 = "deprivation_curiosity_T2",
       label_en = "Deprivation Curiosity",
       label_cn = "剥夺型好奇心"),
  list(m_t3 = "cognitive_curiosity_T3",
       m_t2 = "cognitive_curiosity_T2",
       label_en = "Cognitive Curiosity",
       label_cn = "认知性好奇心"),
  list(m_t3 = "grit_T3",
       m_t2 = "grit_T2",
       label_en = "Grit",
       label_cn = "坚毅")
)

# --- 预测变量组合配置 ---
PREDICTOR_SPECS <- list(
  list(x = "X_op", y = "Y_op",
       x2 = "X_op2", xy = "X_op_Y_op", y2 = "Y_op2",
       sd_pooled = SD_pooled_op,
       label_en = "Overparenting", label_cn = "过度养育", tag = "op"),
  list(x = "X_as", y = "Y_as",
       x2 = "X_as2", xy = "X_as_Y_as", y2 = "Y_as2",
       sd_pooled = SD_pooled_as,
       label_en = "Autonomy Support", label_cn = "自主支持", tag = "as")
)

# --- 批量运行所有 14 个模型 ---
all_results <- list()
model_counter <- 0

for (pred in PREDICTOR_SPECS) {
  for (med in MEDIATOR_SPECS) {
    model_counter <- model_counter + 1
    m_t2_c    <- paste0(med$m_t2, "_c")
    model_key <- paste0(pred$tag, "__", gsub("_T3$", "", med$m_t3))

    label_cn <- paste0(pred$label_cn, " -> ", med$label_cn, " -> 学业倦怠")
    label_en <- paste0(pred$label_en, " -> ", med$label_en, " -> Burnout")

    cat("\n###############################################\n")
    cat("# 模型", model_counter, "/14:", label_cn, "\n")
    cat("###############################################\n\n")

    result <- run_mediated_rsa(
      data    = dat_analysis,
      x_var   = pred$x,   y_var  = pred$y,
      x2_var  = pred$x2,  xy_var = pred$xy,  y2_var = pred$y2,
      m_var   = med$m_t3,
      z_var   = "burnout_T4",
      control_vars_a     = c(CTRL_BASE, m_t2_c),
      control_vars_b     = c(CTRL_BASE, m_t2_c),
      control_vars_total = CTRL_BASE,
      sd_pooled = pred$sd_pooled,
      n_boot    = 5000
    )

    print_results(result, label_cn)
    suggest_alpha_paths(result, pred$x, pred$y, pred$x2, pred$xy, pred$y2)

    out_file <- paste0("results_", pred$tag, "_",
                       gsub("_T3$", "", med$m_t3), ".xlsx")
    export_results(result, out_file, label_cn)

    all_results[[model_key]] <- result
  }
}

cat("\n所有 14 个模型运行完毕。\n")


# =============================================================================
# 第 9 部分: 响应面绘图 (按预测变量组合分别生成 PDF)
# =============================================================================

extract_plot_coefs <- function(model, x_var, y_var, x2_var, xy_var, y2_var) {
  cc <- coef(model)
  c(cc[x_var], cc[y_var], cc[x2_var], cc[xy_var], cc[y2_var], cc["(Intercept)"])
}

for (pred in PREDICTOR_SPECS) {
  pdf_file <- paste0("response_surface_plots_", pred$tag, ".pdf")
  cat("\n生成响应面图:", pdf_file, "...\n")
  pdf(pdf_file, width = 10, height = 8)

  for (med in MEDIATOR_SPECS) {
    model_key <- paste0(pred$tag, "__", gsub("_T3$", "", med$m_t3))
    res <- all_results[[model_key]]
    if (is.null(res)) next

    a_coefs <- extract_plot_coefs(res$model_a,
      pred$x, pred$y, pred$x2, pred$xy, pred$y2)
    t_coefs <- extract_plot_coefs(res$model_total,
      pred$x, pred$y, pred$x2, pred$xy, pred$y2)

    plot_response_surface(a_coefs,
      title = paste0("a path: ", pred$label_en, " -> ", med$label_en),
      xlab = paste0("Parent ", pred$label_en, " (X)"),
      ylab = paste0("Student ", pred$label_en, " (Y)"),
      zlab = med$label_en)

    plot_response_surface(t_coefs,
      title = paste0("Total: ", pred$label_en, " -> Burnout"),
      xlab = paste0("Parent ", pred$label_en, " (X)"),
      ylab = paste0("Student ", pred$label_en, " (Y)"),
      zlab = "Burnout")

    ie_coefs <- a_coefs[1:5] * res$coefs$beta
    ie_b0    <- a_coefs[6]    * res$coefs$beta
    plot_response_surface(c(ie_coefs, ie_b0),
      title = paste0("Indirect: ", pred$label_en, " -> ",
                     med$label_en, " -> Burnout"),
      xlab = paste0("Parent ", pred$label_en, " (X)"),
      ylab = paste0("Student ", pred$label_en, " (Y)"),
      zlab = "Indirect Effect")

    plot_line_effects(a_coefs, pred$sd_pooled,
      title = paste0("a path (", med$label_en, ")"),
      ylab = paste0("Predicted ", med$label_en))
  }

  dev.off()
  cat("已保存:", pdf_file, "\n")
}


# =============================================================================
# 第 10 部分: 描述统计与相关矩阵
# =============================================================================

cat("\n--- 描述统计 ---\n")

desc_vars <- c(
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  "burnout_T2", "burnout_T4",
  "academic_self_efficacy_T2", "academic_self_efficacy_T3",
  "intrinsic_value_T2", "intrinsic_value_T3",
  "socioemotional_curiosity_T2", "socioemotional_curiosity_T3",
  "interest_curiosity_T2", "interest_curiosity_T3",
  "deprivation_curiosity_T2", "deprivation_curiosity_T3",
  "cognitive_curiosity_T2", "cognitive_curiosity_T3",
  "grit_T2", "grit_T3"
)

desc_vars_available <- desc_vars[desc_vars %in% names(dat_analysis)]

desc_stats <- dat_analysis %>%
  select(all_of(desc_vars_available)) %>%
  summarise(across(everything(),
    list(
      N    = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD   = ~sd(., na.rm = TRUE),
      Min  = ~min(., na.rm = TRUE),
      Max  = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "__") %>%
  pivot_wider(names_from = Statistic, values_from = value)

print(desc_stats)

cat("\n--- 相关矩阵 ---\n")
cor_data <- dat_analysis %>%
  select(all_of(desc_vars_available)) %>%
  na.omit()
cat("相关矩阵样本量:", nrow(cor_data), "\n")
print(round(cor(cor_data), 3))


# =============================================================================
# 第 11 部分: 汇总表 — 所有模型的关键间接效应
# =============================================================================

cat("\n--- 所有模型间接效应汇总 ---\n\n")

summary_rows <- list()

for (pred in PREDICTOR_SPECS) {
  for (med in MEDIATOR_SPECS) {
    model_key <- paste0(pred$tag, "__", gsub("_T3$", "", med$m_t3))
    res <- all_results[[model_key]]
    if (is.null(res)) next

    ie_rows <- res$results_table[grep("^IE_", res$results_table$Parameter), ]
    for (j in seq_len(nrow(ie_rows))) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        Predictor = pred$label_en,
        Mediator  = med$label_en,
        N         = res$n,
        Effect    = ie_rows$Parameter[j],
        Estimate  = ie_rows$Estimate[j],
        SE        = ie_rows$SE[j],
        CI_Lo_Perc = ie_rows$CI_Lower_Perc[j],
        CI_Hi_Perc = ie_rows$CI_Upper_Perc[j],
        Sig_Perc   = ie_rows$Sig_Perc[j],
        CI_Lo_BCa  = ie_rows$CI_Lower_BCa[j],
        CI_Hi_BCa  = ie_rows$CI_Upper_BCa[j],
        Sig_BCa    = ie_rows$Sig_BCa[j],
        stringsAsFactors = FALSE
      )
    }
  }
}

summary_table <- do.call(rbind, summary_rows)
print(summary_table, row.names = FALSE)

write_xlsx(list(IE_Summary = summary_table), path = "results_all_indirect_effects_summary.xlsx")
cat("\n汇总表已导出到: results_all_indirect_effects_summary.xlsx\n")


# =============================================================================
# 完成
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  分析完成! 共运行 14 个中介 RSA 模型\n")
cat("=================================================================\n\n")
cat("预测变量组合:\n")
cat("  (1) 过度养育 (parent vs student)\n")
cat("  (2) 自主支持 (parent vs student)\n\n")
cat("中介变量 (T3, 各控制 T2 前测):\n")
cat("  - academic_self_efficacy  (学业自我效能感)\n")
cat("  - intrinsic_value         (内在价值)\n")
cat("  - socioemotional_curiosity (社会情感好奇心)\n")
cat("  - interest_curiosity      (兴趣型好奇心)\n")
cat("  - deprivation_curiosity   (剥夺型好奇心)\n")
cat("  - cognitive_curiosity     (认知性好奇心)\n")
cat("  - grit                    (坚毅)\n\n")
cat("输出文件:\n")
cat("  - results_op_*.xlsx / results_as_*.xlsx  (各模型详细结果)\n")
cat("  - results_all_indirect_effects_summary.xlsx  (间接效应汇总)\n")
cat("  - response_surface_plots_op.pdf  (过度养育响应面图)\n")
cat("  - response_surface_plots_as.pdf  (自主支持响应面图)\n")
cat("\n")
