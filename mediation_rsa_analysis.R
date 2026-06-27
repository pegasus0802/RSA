###############################################################################
# 中介型响应面分析 (Mediated RSA) — 基于 Fu, Dimotakis, & Koopman (2025) 的
# "disaggregated approach"（分解法）
#
# 研究设计:
#   X (学生报告教养) + Y (家长报告教养) → M (T3 中介变量) → Z (T4 学业倦怠)
#
# 核心预测变量组合:
#   (1) 过度养育: X = student_overparenting_T2, Y = parent_overparenting_T2
#   (2) 自主支持: X = student_autonomy_support_T2, Y = parent_autonomy_support_T2
#
# 主中介变量 (T3):
#   - 自我效能感 (self_efficacy_T3)
#   - 内在价值 (intrinsic_value_T3)
#
# 扩展中介变量 (T3):
#   - 兴趣型好奇心 (epistemic_curiosity_interest_T3)
#
# 结果变量 (T4):
#   - 学业倦怠 (burnout_T4)
#
# 参考文献:
#   Fu, S. Q., Dimotakis, N., & Koopman, J. (2025). Mediation testing with
#     polynomial regression: A critical review of extant approaches and a
#     researcher's toolkit for the future. Journal of Applied Psychology,
#     111(1), 1-17.
#   Edwards, J. R., & Parry, M. E. (1993). On the use of polynomial
#     regression equations as an alternative to difference scores in
#     organizational research. Academy of Management Journal, 36(6), 1577-1613.
#   Yao, Y., & Ma, Z. (2023). Toward a holistic perspective of congruence
#     research with the polynomial regression model. Journal of Applied
#     Psychology, 108(3), 446-465.
###############################################################################

# =============================================================================
# 第 0 部分: 加载所需 R 包
# =============================================================================

required_packages <- c(
  "readxl",     # 读取 Excel 数据
  "dplyr",      # 数据处理
  "tidyr",      # 数据整理
  "car",        # linearHypothesis 用于系数线性组合检验
  "boot",       # Bootstrap
  "RSA",        # RSA 响应面绘图 (plotRSA)
  "lavaan",     # RSA 包依赖
  "ggplot2",    # 绑图
  "knitr",      # 结果表格输出
  "kableExtra", # 结果表格美化
  "writexl"     # 输出 Excel 结果
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
# 第 1 部分: 读取数据与变量准备
# =============================================================================

# --- 1.1 读取数据 ---
# 请将路径替换为你的实际数据文件路径
data_path <- "matched_T2_T3_T4_with_T1sex_parent_caregiver_v4.xlsx"
dat <- read_excel(data_path, sheet = "rsa_ready")

# --- 1.2 检查数据维度和关键变量 ---
cat("数据维度:", nrow(dat), "行 ×", ncol(dat), "列\n")
cat("变量名:\n")
print(names(dat))

# --- 1.3 筛选分析样本 ---
# use_T3_core = 1 表示同时有 T2 学生+家长数据和 T3 数据的核心样本
# use_T4_core = 1 表示同时有 T2 学生+家长数据和 T4 数据的核心样本
# 对于完整的中介分析 (T2→T3→T4)，需要同时有 T3 和 T4 数据
dat_analysis <- dat %>%
  filter(use_T3_core == 1 & use_T4_core == 1)

cat("分析样本量:", nrow(dat_analysis), "\n")


# =============================================================================
# 第 2 部分: 变量定义与中心化
# =============================================================================

# --- 2.1 定义量表中点 (midpoint centering) ---
# Fu et al. (2025) 及 Edwards & Parry (1993) 建议使用量表中点中心化
# 这样 x=0, y=0 对应量表中点，一致线和不一致线的解释更直观
#
# 重要: 请根据你的实际量表范围修改这些中点值
# 例如: 如果量表为 1-5 分，中点 = 3
#        如果量表为 1-7 分，中点 = 4

OVERPARENTING_MIDPOINT <- 3     # 过度养育量表中点 (假设 1-5 量表)
AUTONOMY_SUPPORT_MIDPOINT <- 3  # 自主支持量表中点 (假设 1-5 量表)
BURNOUT_MIDPOINT <- 3           # 学业倦怠量表中点 (假设 1-5 量表)
MEDIATOR_MIDPOINT <- 3          # 中介变量量表中点 (假设 1-5 量表)

# --- 2.2 对预测变量进行中心化 ---
dat_analysis <- dat_analysis %>%
  mutate(
    # 过度养育 (midpoint centering)
    X_op = student_overparenting_T2 - OVERPARENTING_MIDPOINT,
    Y_op = parent_overparenting_T2  - OVERPARENTING_MIDPOINT,

    # 自主支持 (midpoint centering)
    X_as = student_autonomy_support_T2 - AUTONOMY_SUPPORT_MIDPOINT,
    Y_as = parent_autonomy_support_T2  - AUTONOMY_SUPPORT_MIDPOINT
  )

# --- 2.3 构造五个多项式项 ---
dat_analysis <- dat_analysis %>%
  mutate(
    # 过度养育的多项式项
    X_op2  = X_op^2,
    X_op_Y_op = X_op * Y_op,
    Y_op2  = Y_op^2,

    # 自主支持的多项式项
    X_as2  = X_as^2,
    X_as_Y_as = X_as * Y_as,
    Y_as2  = Y_as^2
  )

# --- 2.4 计算 pooled SD ---
# Fu et al. 推荐使用 pooled SD = sqrt(SD_x^2 + SD_y^2) 来确定
# 瞬时效应的条件值 (±1 pooled SD)

SD_pooled_op <- sqrt(var(dat_analysis$X_op, na.rm = TRUE) +
                       var(dat_analysis$Y_op, na.rm = TRUE))
SD_pooled_as <- sqrt(var(dat_analysis$X_as, na.rm = TRUE) +
                       var(dat_analysis$Y_as, na.rm = TRUE))

cat("过度养育 pooled SD:", round(SD_pooled_op, 3), "\n")
cat("自主支持 pooled SD:", round(SD_pooled_as, 3), "\n")

# --- 2.5 定义中介变量和结果变量 ---
# 注意: 如果你的数据中尚未包含 T3 中介变量 (如 self_efficacy_T3,
# intrinsic_value_T3 等)，需要先合并 T3 数据。
# 当前数据集中 T3 变量仅有 burnout_T3 及其子维度。
#
# >>> 请取消下面的注释并添加你的 T3 中介变量 <<<
#
# 示例: 如果从另一个文件合并 T3 中介变量:
# t3_mediators <- read_excel("T3_mediator_data.xlsx")
# dat_analysis <- dat_analysis %>%
#   left_join(t3_mediators, by = "final_id")

# 中心化中介变量 (如果需要)
# dat_analysis <- dat_analysis %>%
#   mutate(
#     M_se  = self_efficacy_T3 - MEDIATOR_MIDPOINT,
#     M_iv  = intrinsic_value_T3 - MEDIATOR_MIDPOINT,
#     M_cur = epistemic_curiosity_interest_T3 - MEDIATOR_MIDPOINT
#   )

# 中心化结果变量
dat_analysis <- dat_analysis %>%
  mutate(
    Z_burnout = burnout_T4 - BURNOUT_MIDPOINT
  )

# --- 2.6 定义控制变量 ---
# 将性别转为数值型 (0/1)，SES 保持原样
dat_analysis <- dat_analysis %>%
  mutate(
    sex = as.numeric(as.factor(sex_T1)) - 1,    # 0 = 参考组, 1 = 对比组
    SES = student_SES_T2,
    burnout_T2_c = burnout_T2 - BURNOUT_MIDPOINT # T2 倦怠前测 (控制)
  )


# =============================================================================
# 第 3 部分: 核心分析函数定义
# =============================================================================

# --- 3.1 运行一组完整的中介 RSA 分析 ---
#
# 此函数实现 Fu et al. (2025) disaggregated approach 的完整流程:
#   (1) a 路径: 多项式项 → 中介变量
#   (2) b/c' 路径: 多项式项 + 中介变量 → 结果变量
#   (3) 响应面特征计算 (fit slope/curve, misfit slope/curve)
#   (4) 瞬时效应 (instantaneous effects) 计算
#   (5) 间接效应 = alpha_path × beta_path
#   (6) Bootstrap 置信区间

run_mediated_rsa <- function(data,
                              x_var, y_var,        # 中心化后的 X 和 Y 变量名
                              x2_var, xy_var, y2_var, # 多项式项变量名
                              m_var,               # 中介变量名
                              z_var,               # 结果变量名
                              control_vars = NULL,  # 控制变量名向量
                              sd_pooled,           # pooled SD
                              n_boot = 5000,       # bootstrap 次数
                              conf_level = 0.95,   # 置信区间水平
                              seed = 2024) {       # 随机种子

  set.seed(seed)

  # 移除含缺失值的行 (listwise deletion)
  all_vars <- c(x_var, y_var, x2_var, xy_var, y2_var, m_var, z_var, control_vars)
  dat_complete <- data %>%
    select(all_of(all_vars)) %>%
    na.omit()

  n <- nrow(dat_complete)
  cat("完整案例样本量:", n, "\n")

  # -------------------------------------------------------
  # 3.1.1 构建回归公式
  # -------------------------------------------------------

  # a 路径公式: M ~ X + Y + X² + XY + Y² + controls
  poly_terms <- paste(x_var, y_var, x2_var, xy_var, y2_var, sep = " + ")
  if (!is.null(control_vars) && length(control_vars) > 0) {
    ctrl_terms <- paste(control_vars, collapse = " + ")
    formula_a <- as.formula(paste(m_var, "~", poly_terms, "+", ctrl_terms))
    formula_b <- as.formula(paste(z_var, "~", poly_terms, "+", m_var, "+", ctrl_terms))
    formula_total <- as.formula(paste(z_var, "~", poly_terms, "+", ctrl_terms))
  } else {
    formula_a <- as.formula(paste(m_var, "~", poly_terms))
    formula_b <- as.formula(paste(z_var, "~", poly_terms, "+", m_var))
    formula_total <- as.formula(paste(z_var, "~", poly_terms))
  }

  # -------------------------------------------------------
  # 3.1.2 拟合 OLS 模型
  # -------------------------------------------------------

  # a 路径模型: 多项式项 → 中介变量
  model_a <- lm(formula_a, data = dat_complete)

  # b/c' 路径模型: 多项式项 + 中介变量 → 结果变量
  model_b <- lm(formula_b, data = dat_complete)

  # 总效应模型: 多项式项 → 结果变量 (不含中介)
  model_total <- lm(formula_total, data = dat_complete)

  # -------------------------------------------------------
  # 3.1.3 提取系数
  # -------------------------------------------------------

  coef_a <- coef(model_a)
  coef_b <- coef(model_b)
  coef_total <- coef(model_total)

  # a 路径的五个多项式系数
  a1 <- coef_a[x_var]   # b1α
  a2 <- coef_a[y_var]   # b2α
  a3 <- coef_a[x2_var]  # b3α
  a4 <- coef_a[xy_var]  # b4α
  a5 <- coef_a[y2_var]  # b5α

  # b 路径 (中介 → 结果，控制多项式项后)
  beta <- coef_b[m_var]

  # c' 路径的五个多项式系数 (直接效应)
  c1 <- coef_b[x_var]
  c2 <- coef_b[y_var]
  c3 <- coef_b[x2_var]
  c4 <- coef_b[xy_var]
  c5 <- coef_b[y2_var]

  # 总效应的五个多项式系数
  t1 <- coef_total[x_var]
  t2 <- coef_total[y_var]
  t3 <- coef_total[x2_var]
  t4 <- coef_total[xy_var]
  t5 <- coef_total[y2_var]

  # -------------------------------------------------------
  # 3.1.4 计算响应面特征
  # -------------------------------------------------------

  # --- a 路径 (多项式 → 中介) 的响应面特征 ---
  a_fit_slope  <- a1 + a2                # 一致线斜率
  a_fit_curve  <- a3 + a4 + a5           # 一致线曲率
  a_misfit_slope <- a1 - a2              # 不一致线斜率
  a_misfit_curve <- a3 - a4 + a5         # 不一致线曲率

  # --- c' 路径 (直接效应) 的响应面特征 ---
  c_fit_slope  <- c1 + c2
  c_fit_curve  <- c3 + c4 + c5
  c_misfit_slope <- c1 - c2
  c_misfit_curve <- c3 - c4 + c5

  # --- 总效应的响应面特征 ---
  t_fit_slope  <- t1 + t2
  t_fit_curve  <- t3 + t4 + t5
  t_misfit_slope <- t1 - t2
  t_misfit_curve <- t3 - t4 + t5

  # -------------------------------------------------------
  # 3.1.5 计算瞬时效应 (instantaneous effects)
  # -------------------------------------------------------
  # Fu et al. Appendix C, Eq. (j) 和 (k):
  # 一致线瞬时效应 = (b1+b2) + 2(b3+b4+b5)*x
  # 不一致线瞬时效应 = (b1-b2) + 2(b3-b4+b5)*x
  #
  # 条件值: x = +SD_pooled (excess/高水平一致)
  #          x = -SD_pooled (deficiency/低水平一致)

  # a 路径的瞬时效应
  a_fit_high  <- a_fit_slope + 2 * a_fit_curve * sd_pooled     # 一致线 +1 SD
  a_fit_low   <- a_fit_slope + 2 * a_fit_curve * (-sd_pooled)  # 一致线 -1 SD
  a_misfit_excess <- a_misfit_slope + 2 * a_misfit_curve * sd_pooled     # 不一致线: excess (x>y)
  a_misfit_deficiency <- a_misfit_slope + 2 * a_misfit_curve * (-sd_pooled) # 不一致线: deficiency (x<y)

  # -------------------------------------------------------
  # 3.1.6 计算间接效应 (disaggregated approach)
  # -------------------------------------------------------
  # 间接效应 = alpha_path × beta
  # 每个响应面特征都有对应的间接效应

  # 响应面特征水平的间接效应
  ie_fit_slope  <- a_fit_slope * beta     # 一致线斜率的间接效应
  ie_fit_curve  <- a_fit_curve * beta     # 一致线曲率的间接效应
  ie_misfit_slope <- a_misfit_slope * beta  # 不一致线斜率的间接效应
  ie_misfit_curve <- a_misfit_curve * beta  # 不一致线曲率的间接效应

  # 瞬时效应水平的间接效应
  ie_fit_high <- a_fit_high * beta
  ie_fit_low  <- a_fit_low * beta
  ie_misfit_excess <- a_misfit_excess * beta
  ie_misfit_deficiency <- a_misfit_deficiency * beta

  # -------------------------------------------------------
  # 3.1.7 主轴分析 (Principal Axes)
  # -------------------------------------------------------
  # Edwards & Parry (1993) 公式
  # 第一主轴斜率: p11 = {(b5-b3) + [(b5-b3)^2 + b4^2]^0.5} / b4
  # 第二主轴斜率: p21 = {(b5-b3) - [(b5-b3)^2 + b4^2]^0.5} / b4
  # 驻点: x0 = (b2*b4 - 2*b1*b5) / (4*b3*b5 - b4^2)
  #        y0 = (b1*b4 - 2*b2*b3) / (4*b3*b5 - b4^2)

  compute_principal_axes <- function(b1, b2, b3, b4, b5) {
    denom_sp <- 4 * b3 * b5 - b4^2

    if (abs(denom_sp) < 1e-10) {
      return(list(x0 = NA, y0 = NA, p11 = NA, p10 = NA, p21 = NA, p20 = NA))
    }

    x0 <- (b2 * b4 - 2 * b1 * b5) / denom_sp
    y0 <- (b1 * b4 - 2 * b2 * b3) / denom_sp

    disc <- sqrt((b5 - b3)^2 + b4^2)

    if (abs(b4) < 1e-10) {
      p11 <- NA; p21 <- NA
    } else {
      p11 <- ((b5 - b3) + disc) / b4
      p21 <- ((b5 - b3) - disc) / b4
    }

    p10 <- if (!is.na(p11)) y0 - p11 * x0 else NA
    p20 <- if (!is.na(p21)) y0 - p21 * x0 else NA

    list(x0 = x0, y0 = y0, p11 = p11, p10 = p10, p21 = p21, p20 = p20)
  }

  pa_a <- compute_principal_axes(a1, a2, a3, a4, a5)
  pa_total <- compute_principal_axes(t1, t2, t3, t4, t5)

  # -------------------------------------------------------
  # 3.1.8 Bootstrap 置信区间
  # -------------------------------------------------------

  boot_func <- function(data, indices) {
    d <- data[indices, ]

    # 拟合模型
    mod_a <- lm(formula_a, data = d)
    mod_b <- lm(formula_b, data = d)
    mod_total <- lm(formula_total, data = d)

    ca <- coef(mod_a)
    cb <- coef(mod_b)
    ct <- coef(mod_total)

    # a 路径系数
    ba1 <- ca[x_var]; ba2 <- ca[y_var]; ba3 <- ca[x2_var]
    ba4 <- ca[xy_var]; ba5 <- ca[y2_var]

    # b 路径
    bb <- cb[m_var]

    # c' 路径系数
    bc1 <- cb[x_var]; bc2 <- cb[y_var]; bc3 <- cb[x2_var]
    bc4 <- cb[xy_var]; bc5 <- cb[y2_var]

    # 总效应系数
    bt1 <- ct[x_var]; bt2 <- ct[y_var]; bt3 <- ct[x2_var]
    bt4 <- ct[xy_var]; bt5 <- ct[y2_var]

    # a 路径响应面特征
    a_fs  <- ba1 + ba2
    a_fc  <- ba3 + ba4 + ba5
    a_ms  <- ba1 - ba2
    a_mc  <- ba3 - ba4 + ba5

    # a 路径瞬时效应
    a_fh  <- a_fs + 2 * a_fc * sd_pooled
    a_fl  <- a_fs + 2 * a_fc * (-sd_pooled)
    a_me  <- a_ms + 2 * a_mc * sd_pooled
    a_md  <- a_ms + 2 * a_mc * (-sd_pooled)

    # 间接效应
    ie_fs  <- a_fs * bb
    ie_fc  <- a_fc * bb
    ie_ms  <- a_ms * bb
    ie_mc  <- a_mc * bb
    ie_fh  <- a_fh * bb
    ie_fl  <- a_fl * bb
    ie_me  <- a_me * bb
    ie_md  <- a_md * bb

    # c' 路径响应面特征
    c_fs <- bc1 + bc2
    c_fc <- bc3 + bc4 + bc5
    c_ms <- bc1 - bc2
    c_mc <- bc3 - bc4 + bc5

    # 总效应响应面特征
    t_fs <- bt1 + bt2
    t_fc <- bt3 + bt4 + bt5
    t_ms <- bt1 - bt2
    t_mc <- bt3 - bt4 + bt5

    # 主轴 (a 路径)
    pa <- compute_principal_axes(ba1, ba2, ba3, ba4, ba5)

    c(
      # a 路径系数 (1-5)
      ba1, ba2, ba3, ba4, ba5,
      # b 路径 (6)
      bb,
      # a 路径响应面特征 (7-10)
      a_fs, a_fc, a_ms, a_mc,
      # a 路径瞬时效应 (11-14)
      a_fh, a_fl, a_me, a_md,
      # 间接效应: 特征 (15-18)
      ie_fs, ie_fc, ie_ms, ie_mc,
      # 间接效应: 瞬时 (19-22)
      ie_fh, ie_fl, ie_me, ie_md,
      # c' 路径响应面特征 (23-26)
      c_fs, c_fc, c_ms, c_mc,
      # 总效应响应面特征 (27-30)
      t_fs, t_fc, t_ms, t_mc,
      # 主轴 (31-36)
      pa$x0, pa$y0, pa$p11, pa$p10, pa$p21, pa$p20
    )
  }

  cat("开始 Bootstrap (", n_boot, " 次)...\n")
  boot_results <- boot(dat_complete, boot_func, R = n_boot)
  cat("Bootstrap 完成.\n")

  # 提取 Bootstrap CI
  alpha_ci <- (1 - conf_level) / 2

  get_boot_ci <- function(boot_obj, index) {
    est <- boot_obj$t0[index]
    boot_dist <- boot_obj$t[, index]
    # 使用 percentile method (推荐用于间接效应)
    ci <- quantile(boot_dist, probs = c(alpha_ci, 1 - alpha_ci), na.rm = TRUE)
    se <- sd(boot_dist, na.rm = TRUE)
    c(est = est, se = se, ci_lower = ci[1], ci_upper = ci[2])
  }

  # 提取所有结果
  param_names <- c(
    "a1 (X→M)", "a2 (Y→M)", "a3 (X²→M)", "a4 (XY→M)", "a5 (Y²→M)",
    "beta (M→Z)",
    "a_fit_slope", "a_fit_curve", "a_misfit_slope", "a_misfit_curve",
    "a_fit_high (+1SD)", "a_fit_low (-1SD)",
    "a_misfit_excess (+1SD)", "a_misfit_deficiency (-1SD)",
    "IE_fit_slope", "IE_fit_curve", "IE_misfit_slope", "IE_misfit_curve",
    "IE_fit_high (+1SD)", "IE_fit_low (-1SD)",
    "IE_misfit_excess (+1SD)", "IE_misfit_deficiency (-1SD)",
    "c'_fit_slope", "c'_fit_curve", "c'_misfit_slope", "c'_misfit_curve",
    "total_fit_slope", "total_fit_curve", "total_misfit_slope", "total_misfit_curve",
    "PA_x0", "PA_y0", "PA_p11 (1st axis slope)", "PA_p10 (1st axis intercept)",
    "PA_p21 (2nd axis slope)", "PA_p20 (2nd axis intercept)"
  )

  results_table <- data.frame(
    Parameter = param_names,
    Estimate = NA_real_,
    SE = NA_real_,
    CI_Lower = NA_real_,
    CI_Upper = NA_real_,
    Significant = NA_character_
  )

  for (i in seq_along(param_names)) {
    ci_info <- get_boot_ci(boot_results, i)
    results_table$Estimate[i]  <- round(ci_info["est"], 4)
    results_table$SE[i]        <- round(ci_info["se"], 4)
    results_table$CI_Lower[i]  <- round(ci_info["ci_lower"], 4)
    results_table$CI_Upper[i]  <- round(ci_info["ci_upper"], 4)
    sig <- ifelse(ci_info["ci_lower"] * ci_info["ci_upper"] > 0, "*", "")
    results_table$Significant[i] <- sig
  }

  # -------------------------------------------------------
  # 3.1.9 返回结果
  # -------------------------------------------------------

  list(
    model_a = model_a,
    model_b = model_b,
    model_total = model_total,
    coefs = list(
      a_path = c(a1 = a1, a2 = a2, a3 = a3, a4 = a4, a5 = a5),
      beta = beta,
      c_prime = c(c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5),
      total = c(t1 = t1, t2 = t2, t3 = t3, t4 = t4, t5 = t5)
    ),
    surface_chars = list(
      a_path = c(fit_slope = a_fit_slope, fit_curve = a_fit_curve,
                 misfit_slope = a_misfit_slope, misfit_curve = a_misfit_curve),
      c_prime = c(fit_slope = c_fit_slope, fit_curve = c_fit_curve,
                  misfit_slope = c_misfit_slope, misfit_curve = c_misfit_curve),
      total = c(fit_slope = t_fit_slope, fit_curve = t_fit_curve,
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
    boot_results = boot_results,
    results_table = results_table,
    sd_pooled = sd_pooled,
    n = n
  )
}


# =============================================================================
# 第 4 部分: 响应面绘图函数
# =============================================================================

# --- 4.1 使用 RSA 包的 plotRSA 绘制响应面 ---
plot_response_surface <- function(coefs, title = "",
                                  xlim = c(-2, 2), ylim = c(-2, 2),
                                  zlim = NULL,
                                  xlab = "Student Report (X)",
                                  ylab = "Parent Report (Y)",
                                  zlab = "Outcome") {

  bw <- FALSE

  plotRSA(
    x  = coefs[1],   # b1
    y  = coefs[2],   # b2
    x2 = coefs[3],   # b3
    xy = coefs[4],   # b4
    y2 = coefs[5],   # b5
    b0 = if (length(coefs) >= 6) coefs[6] else 0,
    xlim = xlim, ylim = ylim,
    zlim = if (!is.null(zlim)) zlim else NULL,
    xlab = xlab, ylab = ylab, zlab = zlab,
    type = "3d",
    gridsize = 20,
    showSP = TRUE,
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
    cex.tickLabel = 0.7,
    cex.axesLabel = 0.7
  )
}

# --- 4.2 绘制间接效应响应面 ---
# 间接效应系数 = a 路径系数 × beta
plot_indirect_surface <- function(a_coefs, beta, intercept_a = 0,
                                   title = "Indirect Effect Surface",
                                   xlim = c(-2, 2), ylim = c(-2, 2),
                                   xlab = "Student Report (X)",
                                   ylab = "Parent Report (Y)") {

  # 间接效应的多项式系数
  ie_coefs <- a_coefs * beta
  ie_intercept <- intercept_a * beta

  plot_response_surface(
    coefs = c(ie_coefs, ie_intercept),
    title = title,
    xlim = xlim, ylim = ylim,
    xlab = xlab, ylab = ylab,
    zlab = "Indirect Effect"
  )
}

# --- 4.3 绘制一致线和不一致线的 2D 切面图 ---
plot_line_effects <- function(coefs, sd_pooled,
                               title = "",
                               xlab_fit = "Level of Congruence",
                               xlab_misfit = "Level of Incongruence",
                               ylab = "Predicted Outcome") {

  b1 <- coefs[1]; b2 <- coefs[2]; b3 <- coefs[3]
  b4 <- coefs[4]; b5 <- coefs[5]
  b0 <- if (length(coefs) >= 6) coefs[6] else 0

  x_seq <- seq(-2 * sd_pooled, 2 * sd_pooled, length.out = 100)

  # 一致线: y = x → z = b0 + (b1+b2)x + (b3+b4+b5)x²
  fit_slope <- b1 + b2
  fit_curve <- b3 + b4 + b5
  y_fit <- b0 + fit_slope * x_seq + fit_curve * x_seq^2

  # 不一致线: y = -x → z = b0 + (b1-b2)x + (b3-b4+b5)x²
  misfit_slope <- b1 - b2
  misfit_curve <- b3 - b4 + b5
  y_misfit <- b0 + misfit_slope * x_seq + misfit_curve * x_seq^2

  par(mfrow = c(1, 2))

  # 一致线
  plot(x_seq, y_fit, type = "l", lwd = 2, col = "blue",
       xlab = xlab_fit, ylab = ylab,
       main = paste(title, "- Line of Congruence (y = x)"))
  abline(v = 0, lty = 2, col = "grey50")
  abline(v = c(-sd_pooled, sd_pooled), lty = 3, col = "grey70")
  legend("topleft",
         legend = c(paste("Slope:", round(fit_slope, 3)),
                    paste("Curve:", round(fit_curve, 3))),
         bty = "n", cex = 0.8)

  # 不一致线
  plot(x_seq, y_misfit, type = "l", lwd = 2, col = "red",
       xlab = xlab_misfit, ylab = ylab,
       main = paste(title, "- Line of Incongruence (y = -x)"))
  abline(v = 0, lty = 2, col = "grey50")
  abline(v = c(-sd_pooled, sd_pooled), lty = 3, col = "grey70")
  text(-sd_pooled, max(y_misfit), "Deficiency\n(Y > X)", pos = 4, cex = 0.7)
  text(sd_pooled, max(y_misfit), "Excess\n(X > Y)", pos = 2, cex = 0.7)
  legend("topleft",
         legend = c(paste("Slope:", round(misfit_slope, 3)),
                    paste("Curve:", round(misfit_curve, 3))),
         bty = "n", cex = 0.8)

  par(mfrow = c(1, 1))
}


# =============================================================================
# 第 5 部分: 辅助函数 — linearHypothesis 检验响应面特征
# =============================================================================

# 使用 car::linearHypothesis 检验响应面特征的显著性
test_surface_characteristics <- function(model, x_var, y_var,
                                          x2_var, xy_var, y2_var) {

  # 定义检验
  tests <- list(
    fit_slope  = paste0(x_var, " + ", y_var, " = 0"),
    fit_curve  = paste0(x2_var, " + ", xy_var, " + ", y2_var, " = 0"),
    misfit_slope = paste0(x_var, " - ", y_var, " = 0"),
    misfit_curve = paste0(x2_var, " - ", xy_var, " + ", y2_var, " = 0")
  )

  results <- lapply(names(tests), function(name) {
    test <- linearHypothesis(model, tests[[name]])
    f_val <- test$F[2]
    p_val <- test$`Pr(>F)`[2]

    # 计算估计值
    coefs <- coef(model)
    est <- switch(name,
      fit_slope    = coefs[x_var] + coefs[y_var],
      fit_curve    = coefs[x2_var] + coefs[xy_var] + coefs[y2_var],
      misfit_slope = coefs[x_var] - coefs[y_var],
      misfit_curve = coefs[x2_var] - coefs[xy_var] + coefs[y2_var]
    )

    data.frame(
      Characteristic = name,
      Estimate = round(est, 4),
      F_value = round(f_val, 3),
      p_value = round(p_val, 4),
      Significant = ifelse(p_val < 0.05, "*", "")
    )
  })

  do.call(rbind, results)
}


# =============================================================================
# 第 6 部分: 结果输出函数
# =============================================================================

# --- 6.1 打印格式化结果表 ---
print_results <- function(result, model_label = "Model") {

  cat("\n")
  cat("=================================================================\n")
  cat("  中介 RSA 分析结果:", model_label, "\n")
  cat("=================================================================\n")
  cat("样本量:", result$n, "\n")
  cat("Pooled SD:", round(result$sd_pooled, 3), "\n\n")

  # a 路径模型摘要
  cat("--- a 路径模型 (多项式 → 中介变量) ---\n")
  cat("R² =", round(summary(result$model_a)$r.squared, 4), "\n")
  cat("Adj. R² =", round(summary(result$model_a)$adj.r.squared, 4), "\n")
  print(summary(result$model_a)$coefficients)
  cat("\n")

  # b/c' 路径模型摘要
  cat("--- b/c' 路径模型 (多项式 + 中介 → 结果变量) ---\n")
  cat("R² =", round(summary(result$model_b)$r.squared, 4), "\n")
  cat("Adj. R² =", round(summary(result$model_b)$adj.r.squared, 4), "\n")
  print(summary(result$model_b)$coefficients)
  cat("\n")

  # 总效应模型摘要
  cat("--- 总效应模型 (多项式 → 结果变量, 不含中介) ---\n")
  cat("R² =", round(summary(result$model_total)$r.squared, 4), "\n")
  cat("Adj. R² =", round(summary(result$model_total)$adj.r.squared, 4), "\n")
  print(summary(result$model_total)$coefficients)
  cat("\n")

  # 响应面特征
  cat("--- 响应面特征 (a 路径: 多项式 → 中介) ---\n")
  cat("一致线斜率 (LOC slope, a1+a2):",
      round(result$surface_chars$a_path["fit_slope"], 4), "\n")
  cat("一致线曲率 (LOC curve, a3+a4+a5):",
      round(result$surface_chars$a_path["fit_curve"], 4), "\n")
  cat("不一致线斜率 (LOIC slope, a1-a2):",
      round(result$surface_chars$a_path["misfit_slope"], 4), "\n")
  cat("不一致线曲率 (LOIC curve, a3-a4+a5):",
      round(result$surface_chars$a_path["misfit_curve"], 4), "\n\n")

  # 间接效应表
  cat("--- 间接效应 (Bootstrap", nrow(result$boot_results$t), "次) ---\n")
  ie_rows <- grep("^IE_", result$results_table$Parameter)
  print(result$results_table[ie_rows, ], row.names = FALSE)
  cat("\n")
  cat("注: * 表示 95% Bootstrap CI 不包含 0, 即间接效应显著\n\n")

  # 完整结果表
  cat("--- 完整结果汇总表 ---\n")
  print(result$results_table, row.names = FALSE)
  cat("\n")
}

# --- 6.2 导出结果到 Excel ---
export_results <- function(result, filename, model_label = "Model") {

  # 准备多个 sheet
  sheets <- list()

  # Sheet 1: 完整结果
  sheets[["Full_Results"]] <- result$results_table

  # Sheet 2: a 路径模型系数
  a_summary <- as.data.frame(summary(result$model_a)$coefficients)
  a_summary$Variable <- rownames(a_summary)
  a_summary <- a_summary[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  sheets[["A_Path_Model"]] <- a_summary

  # Sheet 3: b/c' 路径模型系数
  b_summary <- as.data.frame(summary(result$model_b)$coefficients)
  b_summary$Variable <- rownames(b_summary)
  b_summary <- b_summary[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  sheets[["B_Cprime_Model"]] <- b_summary

  # Sheet 4: 总效应模型系数
  t_summary <- as.data.frame(summary(result$model_total)$coefficients)
  t_summary$Variable <- rownames(t_summary)
  t_summary <- t_summary[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  sheets[["Total_Effect_Model"]] <- t_summary

  # Sheet 5: 模型拟合信息
  fit_info <- data.frame(
    Model = c("a_path", "b_cprime", "total"),
    R_squared = c(
      summary(result$model_a)$r.squared,
      summary(result$model_b)$r.squared,
      summary(result$model_total)$r.squared
    ),
    Adj_R_squared = c(
      summary(result$model_a)$adj.r.squared,
      summary(result$model_b)$adj.r.squared,
      summary(result$model_total)$adj.r.squared
    ),
    N = result$n
  )
  sheets[["Model_Fit"]] <- fit_info

  write_xlsx(sheets, path = filename)
  cat("结果已导出到:", filename, "\n")
}


# =============================================================================
# 第 7 部分: 确定 alpha 路径的决策逻辑
# =============================================================================

# Fu et al. (2025) 指出, 选择哪些效应作为 alpha 路径需要根据
# 响应面特征的显著性来决定:
#
# (1) 一致线 (LOC):
#     - 如果仅斜率显著 (无曲率): alpha = fit_slope (a1+a2)
#     - 如果仅曲率显著 (无斜率): alpha = fit_curve (a3+a4+a5)
#     - 如果斜率和曲率均显著: 需要报告 ±1 SD 的瞬时效应
#       alpha_high = fit_slope + 2*fit_curve*SD_pooled
#       alpha_low  = fit_slope + 2*fit_curve*(-SD_pooled)
#
# (2) 不一致线 (LOIC):
#     - 如果仅曲率显著 (无斜率): alpha = misfit_curve (a3-a4+a5)
#       → 这是最常见的 "一致 vs 不一致" 效应
#     - 如果仅斜率显著 (无曲率): alpha = misfit_slope (a1-a2)
#       → 方向性效应 (excess vs deficiency)
#     - 如果斜率和曲率均显著: 需要报告 ±1 SD 的瞬时效应
#       alpha_excess     = misfit_slope + 2*misfit_curve*SD_pooled
#       alpha_deficiency = misfit_slope + 2*misfit_curve*(-SD_pooled)
#
# 此函数自动判断并返回建议的 alpha 路径

suggest_alpha_paths <- function(result, x_var, y_var, x2_var, xy_var, y2_var) {

  cat("\n--- Alpha 路径选择建议 ---\n\n")

  # 检验 a 路径模型的响应面特征
  surface_tests <- test_surface_characteristics(
    result$model_a, x_var, y_var, x2_var, xy_var, y2_var
  )

  cat("a 路径响应面特征检验:\n")
  print(surface_tests, row.names = FALSE)
  cat("\n")

  fit_slope_sig <- surface_tests$p_value[surface_tests$Characteristic == "fit_slope"] < 0.05
  fit_curve_sig <- surface_tests$p_value[surface_tests$Characteristic == "fit_curve"] < 0.05
  misfit_slope_sig <- surface_tests$p_value[surface_tests$Characteristic == "misfit_slope"] < 0.05
  misfit_curve_sig <- surface_tests$p_value[surface_tests$Characteristic == "misfit_curve"] < 0.05

  cat("一致线 (LOC):\n")
  if (fit_slope_sig && !fit_curve_sig) {
    cat("  → 仅斜率显著: 使用 fit_slope 作为 alpha 路径\n")
    cat("  → 间接效应 = fit_slope × beta\n")
  } else if (!fit_slope_sig && fit_curve_sig) {
    cat("  → 仅曲率显著: 使用 fit_curve 作为 alpha 路径\n")
    cat("  → 间接效应 = fit_curve × beta\n")
  } else if (fit_slope_sig && fit_curve_sig) {
    cat("  → 斜率和曲率均显著: 需要报告 ±1 SD 的瞬时效应\n")
    cat("  → alpha_high = fit_slope + 2*fit_curve*SD_pooled\n")
    cat("  → alpha_low  = fit_slope - 2*fit_curve*SD_pooled\n")
    cat("  → 分别计算两个间接效应\n")
  } else {
    cat("  → 斜率和曲率均不显著: 一致线上无显著 alpha 路径\n")
  }

  cat("\n不一致线 (LOIC):\n")
  if (!misfit_slope_sig && misfit_curve_sig) {
    cat("  → 仅曲率显著: 使用 misfit_curve 作为 alpha 路径\n")
    cat("  → 间接效应 = misfit_curve × beta (一致 vs 不一致效应)\n")
  } else if (misfit_slope_sig && !misfit_curve_sig) {
    cat("  → 仅斜率显著: 使用 misfit_slope 作为 alpha 路径\n")
    cat("  → 间接效应 = misfit_slope × beta (方向性效应)\n")
  } else if (misfit_slope_sig && misfit_curve_sig) {
    cat("  → 斜率和曲率均显著: 需要报告 ±1 SD 的瞬时效应\n")
    cat("  → alpha_excess     = misfit_slope + 2*misfit_curve*SD_pooled\n")
    cat("  → alpha_deficiency = misfit_slope - 2*misfit_curve*SD_pooled\n")
    cat("  → 分别计算 excess 和 deficiency 区域的间接效应\n")
  } else {
    cat("  → 斜率和曲率均不显著: 不一致线上无显著 alpha 路径\n")
  }

  cat("\n")

  invisible(surface_tests)
}


# =============================================================================
# 第 8 部分: 运行分析
# =============================================================================
#
# 以下代码展示如何针对具体的预测变量-中介-结果组合运行分析。
# 由于当前数据集中 T3 中介变量 (self_efficacy_T3 等) 尚未合并，
# 这里提供两种运行方式:
#   (A) 使用已有变量做演示 (low_efficacy_T3 → burnout_T4)
#   (B) 使用占位符，等你合并 T3 中介数据后取消注释即可运行
#

# ========================================
# 方式 A: 使用已有变量做演示
# ========================================
# 注意: low_efficacy_T3 是学业倦怠的一个子维度（低效能感），
# 虽然理论上不是最理想的中介选择，但可以用来验证代码运行正确性

cat("\n###############################################\n")
cat("# 演示分析: 过度养育 → low_efficacy_T3 → burnout_T4\n")
cat("###############################################\n\n")

# 中心化 low_efficacy_T3 作为演示中介
dat_analysis$M_demo <- dat_analysis$low_efficacy_T3 - BURNOUT_MIDPOINT

# 运行分析
result_demo <- run_mediated_rsa(
  data = dat_analysis,
  x_var = "X_op",         # 学生报告过度养育 (中心化)
  y_var = "Y_op",         # 家长报告过度养育 (中心化)
  x2_var = "X_op2",       # X²
  xy_var = "X_op_Y_op",   # XY
  y2_var = "Y_op2",       # Y²
  m_var = "M_demo",       # 中介: low_efficacy_T3 (演示用)
  z_var = "Z_burnout",    # 结果: burnout_T4
  control_vars = c("sex", "SES", "burnout_T2_c"),  # 控制变量
  sd_pooled = SD_pooled_op,
  n_boot = 5000,          # 正式分析建议 5000-10000 次
  conf_level = 0.95
)

# 打印结果
print_results(result_demo, model_label = "演示: 过度养育 → low_efficacy_T3 → burnout_T4")

# Alpha 路径选择建议
suggest_alpha_paths(result_demo, "X_op", "Y_op", "X_op2", "X_op_Y_op", "Y_op2")

# 导出结果
export_results(result_demo, "results_demo_overparenting_lowefficacy.xlsx")


# ========================================
# 方式 B: 正式分析模板 (需要合并 T3 中介数据后运行)
# ========================================
#
# >>> 取消下面的注释块来运行正式分析 <<<
#

# ----------------------------------------
# 主模型 1: 过度养育 → 自我效能感 → 学业倦怠
# ----------------------------------------
#
# cat("\n###############################################\n")
# cat("# 主模型 1: 过度养育 → self_efficacy_T3 → burnout_T4\n")
# cat("###############################################\n\n")
#
# result_op_se <- run_mediated_rsa(
#   data = dat_analysis,
#   x_var = "X_op", y_var = "Y_op",
#   x2_var = "X_op2", xy_var = "X_op_Y_op", y2_var = "Y_op2",
#   m_var = "M_se",          # 自我效能感 (中心化)
#   z_var = "Z_burnout",
#   control_vars = c("sex", "SES", "burnout_T2_c"),
#   sd_pooled = SD_pooled_op,
#   n_boot = 5000
# )
# print_results(result_op_se, "主模型 1: 过度养育 → 自我效能感 → 学业倦怠")
# suggest_alpha_paths(result_op_se, "X_op", "Y_op", "X_op2", "X_op_Y_op", "Y_op2")
# export_results(result_op_se, "results_overparenting_selfefficacy.xlsx")


# ----------------------------------------
# 主模型 2: 过度养育 → 内在价值 → 学业倦怠
# ----------------------------------------
#
# result_op_iv <- run_mediated_rsa(
#   data = dat_analysis,
#   x_var = "X_op", y_var = "Y_op",
#   x2_var = "X_op2", xy_var = "X_op_Y_op", y2_var = "Y_op2",
#   m_var = "M_iv",          # 内在价值 (中心化)
#   z_var = "Z_burnout",
#   control_vars = c("sex", "SES", "burnout_T2_c"),
#   sd_pooled = SD_pooled_op,
#   n_boot = 5000
# )
# print_results(result_op_iv, "主模型 2: 过度养育 → 内在价值 → 学业倦怠")
# suggest_alpha_paths(result_op_iv, "X_op", "Y_op", "X_op2", "X_op_Y_op", "Y_op2")
# export_results(result_op_iv, "results_overparenting_intrinsicvalue.xlsx")


# ----------------------------------------
# 主模型 3: 自主支持 → 自我效能感 → 学业倦怠
# ----------------------------------------
#
# result_as_se <- run_mediated_rsa(
#   data = dat_analysis,
#   x_var = "X_as", y_var = "Y_as",
#   x2_var = "X_as2", xy_var = "X_as_Y_as", y2_var = "Y_as2",
#   m_var = "M_se",
#   z_var = "Z_burnout",
#   control_vars = c("sex", "SES", "burnout_T2_c"),
#   sd_pooled = SD_pooled_as,
#   n_boot = 5000
# )
# print_results(result_as_se, "主模型 3: 自主支持 → 自我效能感 → 学业倦怠")
# suggest_alpha_paths(result_as_se, "X_as", "Y_as", "X_as2", "X_as_Y_as", "Y_as2")
# export_results(result_as_se, "results_autonomysupport_selfefficacy.xlsx")


# ----------------------------------------
# 主模型 4: 自主支持 → 内在价值 → 学业倦怠
# ----------------------------------------
#
# result_as_iv <- run_mediated_rsa(
#   data = dat_analysis,
#   x_var = "X_as", y_var = "Y_as",
#   x2_var = "X_as2", xy_var = "X_as_Y_as", y2_var = "Y_as2",
#   m_var = "M_iv",
#   z_var = "Z_burnout",
#   control_vars = c("sex", "SES", "burnout_T2_c"),
#   sd_pooled = SD_pooled_as,
#   n_boot = 5000
# )
# print_results(result_as_iv, "主模型 4: 自主支持 → 内在价值 → 学业倦怠")
# suggest_alpha_paths(result_as_iv, "X_as", "Y_as", "X_as2", "X_as_Y_as", "Y_as2")
# export_results(result_as_iv, "results_autonomysupport_intrinsicvalue.xlsx")


# ----------------------------------------
# 扩展模型: 自主支持 → 兴趣型好奇心 → 学业倦怠
# ----------------------------------------
#
# result_as_cur <- run_mediated_rsa(
#   data = dat_analysis,
#   x_var = "X_as", y_var = "Y_as",
#   x2_var = "X_as2", xy_var = "X_as_Y_as", y2_var = "Y_as2",
#   m_var = "M_cur",          # 兴趣型好奇心 (中心化)
#   z_var = "Z_burnout",
#   control_vars = c("sex", "SES", "burnout_T2_c"),
#   sd_pooled = SD_pooled_as,
#   n_boot = 5000
# )
# print_results(result_as_cur, "扩展: 自主支持 → 兴趣型好奇心 → 学业倦怠")
# suggest_alpha_paths(result_as_cur, "X_as", "Y_as", "X_as2", "X_as_Y_as", "Y_as2")
# export_results(result_as_cur, "results_autonomysupport_curiosity.xlsx")


# =============================================================================
# 第 9 部分: 响应面绘图 (在分析结果生成后运行)
# =============================================================================

# --- 9.1 绘制 a 路径 (多项式 → 中介) 的响应面 ---
# 以演示模型为例
cat("\n生成响应面图...\n")

# a 路径响应面
a_coefs_demo <- c(
  coef(result_demo$model_a)["X_op"],
  coef(result_demo$model_a)["Y_op"],
  coef(result_demo$model_a)["X_op2"],
  coef(result_demo$model_a)["X_op_Y_op"],
  coef(result_demo$model_a)["Y_op2"],
  coef(result_demo$model_a)["(Intercept)"]
)

# 总效应响应面
t_coefs_demo <- c(
  coef(result_demo$model_total)["X_op"],
  coef(result_demo$model_total)["Y_op"],
  coef(result_demo$model_total)["X_op2"],
  coef(result_demo$model_total)["X_op_Y_op"],
  coef(result_demo$model_total)["Y_op2"],
  coef(result_demo$model_total)["(Intercept)"]
)

# c' 路径 (直接效应) 响应面
c_coefs_demo <- c(
  coef(result_demo$model_b)["X_op"],
  coef(result_demo$model_b)["Y_op"],
  coef(result_demo$model_b)["X_op2"],
  coef(result_demo$model_b)["X_op_Y_op"],
  coef(result_demo$model_b)["Y_op2"],
  coef(result_demo$model_b)["(Intercept)"]
)

# 绘图
pdf("response_surface_plots_demo.pdf", width = 10, height = 8)

plot_response_surface(
  a_coefs_demo,
  title = "a path: Overparenting -> Mediator",
  xlab = "Student Overparenting (X)", ylab = "Parent Overparenting (Y)",
  zlab = "Low Efficacy (M)"
)

plot_response_surface(
  t_coefs_demo,
  title = "Total Effect: Overparenting -> Burnout",
  xlab = "Student Overparenting (X)", ylab = "Parent Overparenting (Y)",
  zlab = "Burnout (Z)"
)

plot_response_surface(
  c_coefs_demo,
  title = "Direct Effect (c'): Overparenting -> Burnout",
  xlab = "Student Overparenting (X)", ylab = "Parent Overparenting (Y)",
  zlab = "Burnout (Z)"
)

# 间接效应响应面
ie_coefs_demo <- a_coefs_demo[1:5] * result_demo$coefs$beta
ie_intercept <- a_coefs_demo[6] * result_demo$coefs$beta
plot_response_surface(
  c(ie_coefs_demo, ie_intercept),
  title = "Indirect Effect Surface",
  xlab = "Student Overparenting (X)", ylab = "Parent Overparenting (Y)",
  zlab = "Indirect Effect on Burnout"
)

# 一致线和不一致线 2D 切面
plot_line_effects(
  a_coefs_demo, SD_pooled_op,
  title = "a path",
  ylab = "Predicted Low Efficacy"
)

plot_line_effects(
  t_coefs_demo, SD_pooled_op,
  title = "Total Effect",
  ylab = "Predicted Burnout"
)

dev.off()
cat("响应面图已保存到: response_surface_plots_demo.pdf\n")


# =============================================================================
# 第 10 部分: 描述统计与相关矩阵
# =============================================================================

cat("\n--- 描述统计 ---\n")

desc_vars <- c(
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  "burnout_T2", "burnout_T3", "burnout_T4",
  "low_efficacy_T3"
  # 合并 T3 中介变量后添加:
  # "self_efficacy_T3", "intrinsic_value_T3",
  # "epistemic_curiosity_interest_T3"
)

desc_vars_available <- desc_vars[desc_vars %in% names(dat_analysis)]

desc_stats <- dat_analysis %>%
  select(all_of(desc_vars_available)) %>%
  summarise(across(everything(),
    list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "__") %>%
  pivot_wider(names_from = Statistic, values_from = value)

print(desc_stats)

# 相关矩阵
cat("\n--- 相关矩阵 ---\n")
cor_data <- dat_analysis %>%
  select(all_of(desc_vars_available)) %>%
  na.omit()
cor_matrix <- cor(cor_data)
print(round(cor_matrix, 3))


# =============================================================================
# 第 11 部分: 补充分析 — 多重中介模型比较
# =============================================================================
#
# 如果需要比较多个中介变量的间接效应大小:
#
# compare_indirect_effects <- function(result1, result2,
#                                       effect_name = "IE_misfit_curve") {
#   idx1 <- which(result1$results_table$Parameter == effect_name)
#   idx2 <- which(result2$results_table$Parameter == effect_name)
#
#   ie1 <- result1$results_table$Estimate[idx1]
#   ie2 <- result2$results_table$Estimate[idx2]
#
#   # 从 bootstrap 分布计算差异的 CI
#   ie1_boot <- result1$boot_results$t[, 15 + which(
#     c("IE_fit_slope", "IE_fit_curve", "IE_misfit_slope", "IE_misfit_curve",
#       "IE_fit_high (+1SD)", "IE_fit_low (-1SD)",
#       "IE_misfit_excess (+1SD)", "IE_misfit_deficiency (-1SD)") ==
#     gsub("^IE_", "IE_", effect_name)) - 1]
#   ie2_boot <- result2$boot_results$t[, 15 + which(
#     c("IE_fit_slope", "IE_fit_curve", "IE_misfit_slope", "IE_misfit_curve",
#       "IE_fit_high (+1SD)", "IE_fit_low (-1SD)",
#       "IE_misfit_excess (+1SD)", "IE_misfit_deficiency (-1SD)") ==
#     gsub("^IE_", "IE_", effect_name)) - 1]
#
#   diff_boot <- ie1_boot - ie2_boot
#   ci_diff <- quantile(diff_boot, c(0.025, 0.975), na.rm = TRUE)
#
#   cat("间接效应差异 (模型1 - 模型2):\n")
#   cat("  效应类型:", effect_name, "\n")
#   cat("  模型1 IE:", round(ie1, 4), "\n")
#   cat("  模型2 IE:", round(ie2, 4), "\n")
#   cat("  差异:", round(ie1 - ie2, 4), "\n")
#   cat("  95% CI: [", round(ci_diff[1], 4), ",", round(ci_diff[2], 4), "]\n")
#   cat("  显著:", ifelse(ci_diff[1] * ci_diff[2] > 0, "是", "否"), "\n")
# }


# =============================================================================
# 完成
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  分析完成!\n")
cat("=================================================================\n")
cat("\n")
cat("重要提示:\n")
cat("  1. 当前演示使用 low_efficacy_T3 作为中介变量 (仅为代码验证)。\n")
cat("  2. 正式分析请合并 T3 中介变量数据后取消第 8 部分方式 B 的注释。\n")
cat("  3. 量表中点值 (MIDPOINT) 请根据实际量表范围修改。\n")
cat("  4. Bootstrap 次数建议正式分析使用 5000-10000 次。\n")
cat("  5. 结果解释请参考 Fu et al. (2025) 论文中 Table 2 的格式。\n")
cat("\n")
cat("输出文件:\n")
cat("  - results_demo_overparenting_lowefficacy.xlsx (Excel 结果表)\n")
cat("  - response_surface_plots_demo.pdf (响应面图)\n")
cat("\n")
