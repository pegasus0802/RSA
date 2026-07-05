###############################################################################
# precheck_congruence_vif_attrition.R
#
# RSA 前置检验 (独立脚本, 不修改 mediation_rsa_analysis.R):
#   A. 一致/不一致比例
#      主口径:   Shanock et al. (2010) / Fleenor et al. (1996) —
#                两预测变量各自 z 标准化, |z_X - z_Y| > 0.5 判为不一致
#      敏感性口径: 原始差值 |X - Y| > 0.5 * pooled SD —
#                与主分析 ±1 pooled SD 瞬时效应求值点同一尺度;
#                RSA 包 print 输出采用相近口径 (合并共同均值/SD 标准化),
#                但该口径仅见于包文档, 故作敏感性而非主口径
#      计算样本: (i) 每对预测变量完整样本; (ii) 14 个模型 listwise 样本
#   B. 数据覆盖: bagplot + LOC/LOIC 线 ±1 pooled SD 四个求值点
#      是否落在 bag (内 50%) / loop (fence) / 凸包内
#   C. VIF: 14 个 a 路径 + 14 个 b 路径 + 2 个总效应模型
#   D. 流失分析: 设计性筛选流程表 + 标志组合频数 +
#      纳入 vs 排除的 T1/T2 变量比较 (Welch t, Cohen d) +
#      性别卡方 + 分析样本内缺失率
#
# 输出:
#   results_precheck_congruence_vif_attrition.xlsx (9 sheets)
#   precheck_bagplots.pdf (2 页)
#
# 以下部分为对主脚本的逐字/等价复刻, 若主脚本相应部分变更, 本脚本须同步:
#   设计性筛选 / midpoint centering / 控制变量中心化 / 中介 T2 中心化 /
#   pooled SD / CTRL_BASE / MEDIATOR_SPECS / 模型特异性 listwise
###############################################################################

# ==== 0. 包 ====
required_packages <- c("readxl", "dplyr", "writexl", "aplpack", "car")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
  library(pkg, character.only = TRUE)
}
options(scipen = 999)
set.seed(2024)   # compute.bagplot 在大样本下可能采用近似抽样

# ==== 1. 读取数据 ====
DATA_FILE  <- "matched_T2_T3_T4_with_T1sex_parent_caregiver_v4_plus_mediators.xlsx"
SHEET_NAME <- "rsa_ready_plus_mediators"

if (!file.exists(DATA_FILE)) {
  hits <- list.files(
    pattern = "^matched_T2_T3_T4_with_T1sex_parent_caregiver_v4_plus_mediators")
  if (length(hits) == 1) {
    DATA_FILE <- hits[1]
    cat("[INFO] 使用自动定位的数据文件:", DATA_FILE, "\n")
  } else {
    stop("未找到唯一数据文件, 匹配结果: ",
         paste(hits, collapse = ", "))
  }
}

available_sheets <- readxl::excel_sheets(DATA_FILE)
if (!SHEET_NAME %in% available_sheets) {
  stop("Sheet '", SHEET_NAME, "' 未找到。可用 sheets: ",
       paste(available_sheets, collapse = ", "))
}
dat <- read_excel(DATA_FILE, sheet = SHEET_NAME)
cat("数据文件:", DATA_FILE, " | 维度:", nrow(dat), "x", ncol(dat), "\n")

REQUIRED_VARS <- c(
  "final_id", "sex_T1", "parent_SES_T2",
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  "burnout_T2", "burnout_T3", "burnout_T4",
  "has_T2_parent", "has_T3", "has_T4", "caregiver_match_T2",
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

# ---- 标志变量稳健转换 (逐字复刻主脚本 to01) ----
to01 <- function(x) {
  x_chr <- trimws(as.character(x))
  dplyr::case_when(
    x_chr %in% c("TRUE", "True", "true", "T", "1", "是", "yes", "YES", "Y", "y") ~ 1,
    x_chr %in% c("FALSE", "False", "false", "F", "0", "否", "no", "NO", "N", "n") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}
dat <- dat %>%
  mutate(
    has_T2_parent      = to01(has_T2_parent),
    has_T3             = to01(has_T3),
    has_T4             = to01(has_T4),
    caregiver_match_T2 = to01(caregiver_match_T2)
  )

# ==== 2. 设计性筛选 (复刻主脚本 1.4) ====
dat_analysis <- dat %>%
  filter(has_T2_parent == 1, has_T3 == 1, has_T4 == 1,
         caregiver_match_T2 == 1)
cat("设计性筛选后样本量:", nrow(dat_analysis), "\n")
stopifnot(nrow(dat_analysis) > 0)

# ==== 3. 中心化 / 多项式项 / pooled SD (复刻主脚本第 2 部分) ====
OVERPARENTING_MIDPOINT    <- 3   # 过度养育 1-5 量表
AUTONOMY_SUPPORT_MIDPOINT <- 4   # 自主支持 1-7 量表

dat_analysis <- dat_analysis %>%
  mutate(
    X_op = parent_overparenting_T2  - OVERPARENTING_MIDPOINT,
    Y_op = student_overparenting_T2 - OVERPARENTING_MIDPOINT,
    X_as = parent_autonomy_support_T2  - AUTONOMY_SUPPORT_MIDPOINT,
    Y_as = student_autonomy_support_T2 - AUTONOMY_SUPPORT_MIDPOINT,
    X_op2 = X_op^2, X_op_Y_op = X_op * Y_op, Y_op2 = Y_op^2,
    X_as2 = X_as^2, X_as_Y_as = X_as * Y_as, Y_as2 = Y_as^2,
    sex_T1_c = as.numeric(sex_T1) -
      mean(as.numeric(sex_T1), na.rm = TRUE),
    ses_control_c = as.numeric(parent_SES_T2) -
      mean(as.numeric(parent_SES_T2), na.rm = TRUE),
    baseline_burnout_c = as.numeric(burnout_T2) -
      mean(as.numeric(burnout_T2), na.rm = TRUE)
  )

MEDIATOR_T2_VARS <- c(
  "academic_self_efficacy_T2", "intrinsic_value_T2",
  "socioemotional_curiosity_T2", "interest_curiosity_T2",
  "deprivation_curiosity_T2", "cognitive_curiosity_T2", "grit_T2"
)
for (v in MEDIATOR_T2_VARS) {
  dat_analysis[[paste0(v, "_c")]] <- as.numeric(dat_analysis[[v]]) -
    mean(as.numeric(dat_analysis[[v]]), na.rm = TRUE)
}

SD_pooled_op <- sqrt((var(dat_analysis$X_op, na.rm = TRUE) +
                      var(dat_analysis$Y_op, na.rm = TRUE)) / 2)
SD_pooled_as <- sqrt((var(dat_analysis$X_as, na.rm = TRUE) +
                      var(dat_analysis$Y_as, na.rm = TRUE)) / 2)
cat("过度养育 pooled SD:", round(SD_pooled_op, 3),
    "| 自主支持 pooled SD:", round(SD_pooled_as, 3), "\n")
cat("[人工比对项] 上述两值应与 mediation_rsa_analysis.R 运行日志一致。\n")

CTRL_BASE <- c("sex_T1_c", "ses_control_c", "baseline_burnout_c")

MEDIATOR_SPECS <- list(
  list(m_t3 = "academic_self_efficacy_T3",
       m_t2 = "academic_self_efficacy_T2",
       label_en = "Academic Self-Efficacy", label_cn = "学业自我效能感"),
  list(m_t3 = "intrinsic_value_T3",
       m_t2 = "intrinsic_value_T2",
       label_en = "Intrinsic Value", label_cn = "内在价值"),
  list(m_t3 = "socioemotional_curiosity_T3",
       m_t2 = "socioemotional_curiosity_T2",
       label_en = "Socioemotional Curiosity", label_cn = "社会情感好奇心"),
  list(m_t3 = "interest_curiosity_T3",
       m_t2 = "interest_curiosity_T2",
       label_en = "Interest Curiosity", label_cn = "兴趣型好奇心"),
  list(m_t3 = "deprivation_curiosity_T3",
       m_t2 = "deprivation_curiosity_T2",
       label_en = "Deprivation Curiosity", label_cn = "剥夺型好奇心"),
  list(m_t3 = "cognitive_curiosity_T3",
       m_t2 = "cognitive_curiosity_T2",
       label_en = "Cognitive Curiosity", label_cn = "认知性好奇心"),
  list(m_t3 = "grit_T3",
       m_t2 = "grit_T2",
       label_en = "Grit", label_cn = "坚毅")
)

PREDICTOR_SPECS_PC <- list(
  list(x = "X_op", y = "Y_op",
       x2 = "X_op2", xy = "X_op_Y_op", y2 = "Y_op2",
       sd_pooled = SD_pooled_op, plot_lim = c(-2, 2),
       label_en = "Overparenting", label_cn = "过度养育", tag = "op"),
  list(x = "X_as", y = "Y_as",
       x2 = "X_as2", xy = "X_as_Y_as", y2 = "Y_as2",
       sd_pooled = SD_pooled_as, plot_lim = c(-3, 3),
       label_en = "Autonomy Support", label_cn = "自主支持", tag = "as")
)

# ==== 4. 分类口径与辅助函数 ====

# 主口径: Shanock et al. (2010) / Fleenor et al. (1996)
# 各自 z 标准化 (在传入的样本内计算均值与 SD), |z_X - z_Y| > 0.5 判为不一致。
# 注意: 该口径抹掉信息源均值差 (相对一致); 系统性均值差另见配对 t / 描述统计。
classify_shanock <- function(x, y) {
  zx <- (x - mean(x)) / stats::sd(x)
  zy <- (y - mean(y)) / stats::sd(y)
  d  <- zx - zy
  cut(d, breaks = c(-Inf, -0.5, 0.5, Inf),
      labels = c("student_higher", "congruent", "parent_higher"))
}

# 敏感性口径: 原始差值 vs 0.5 * 全局 pooled SD (与主分析求值点同一常数)
classify_pooled <- function(x, y, sd_pooled) {
  d  <- x - y
  c0 <- 0.5 * sd_pooled
  cut(d, breaks = c(-Inf, -c0, c0, Inf),
      labels = c("student_higher", "congruent", "parent_higher"))
}

prop3 <- function(cls) {
  tab <- table(factor(cls,
    levels = c("student_higher", "congruent", "parent_higher")))
  round(100 * as.numeric(tab) / sum(tab), 2)
}

below10_flag <- function(p3) {
  cats <- c("student_higher", "congruent", "parent_higher")
  low  <- cats[p3 < 10]
  if (length(low) == 0) "" else paste(low, collapse = "; ")
}

# 射线法点在多边形内判断; poly 为两列坐标矩阵 (顶点按序)
point_in_poly <- function(px, py, poly) {
  n <- nrow(poly); inside <- FALSE; j <- n
  for (i in seq_len(n)) {
    xi <- poly[i, 1]; yi <- poly[i, 2]
    xj <- poly[j, 1]; yj <- poly[j, 2]
    if (((yi > py) != (yj > py)) &&
        (px < (xj - xi) * (py - yi) / (yj - yi) + xi)) {
      inside <- !inside
    }
    j <- i
  }
  inside
}

# ==== 5A. 每对预测变量: 完整样本比例 + 求值点覆盖 + bagplot ====
pair_rows     <- list()
coverage_rows <- list()

pdf("precheck_bagplots.pdf", width = 7, height = 7)

for (pred in PREDICTOR_SPECS_PC) {
  d_pair <- dat_analysis %>%
    select(all_of(c(pred$x, pred$y))) %>%
    na.omit()
  xv <- d_pair[[pred$x]]
  yv <- d_pair[[pred$y]]

  ps <- prop3(classify_shanock(xv, yv))
  pp <- prop3(classify_pooled(xv, yv, pred$sd_pooled))

  pair_rows[[pred$tag]] <- data.frame(
    Predictor_Set = pred$label_en, N = nrow(d_pair),
    Shanock_Student_higher = ps[1], Shanock_Congruent = ps[2],
    Shanock_Parent_higher = ps[3],
    PooledSD_Student_higher = pp[1], PooledSD_Congruent = pp[2],
    PooledSD_Parent_higher = pp[3],
    Below_10pct_Shanock = below10_flag(ps),
    stringsAsFactors = FALSE
  )
  if (nzchar(below10_flag(ps))) {
    cat("[WARN]", pred$label_en,
        "- Shanock 口径下占比 <10% 的类别:", below10_flag(ps), "\n")
  }

  # ±1 pooled SD 求值点 (LOC 两点 + LOIC 两点; 与主分析瞬时效应一致)
  s  <- pred$sd_pooled
  ep <- data.frame(
    Point = c("LOC +1SD (X=Y=+s)", "LOC -1SD (X=Y=-s)",
              "LOIC excess (X=+s, Y=-s)", "LOIC deficiency (X=-s, Y=+s)"),
    px = c( s, -s,  s, -s),
    py = c( s, -s, -s,  s),
    stringsAsFactors = FALSE
  )

  bp <- tryCatch(aplpack::compute.bagplot(xv, yv), error = function(e) NULL)
  hull_bag <- if (!is.null(bp) && is.matrix(bp$hull.bag) &&
                  nrow(bp$hull.bag) >= 3) bp$hull.bag else NULL
  hull_loop <- if (!is.null(bp) && is.matrix(bp$hull.loop) &&
                   nrow(bp$hull.loop) >= 3) bp$hull.loop else NULL
  ch <- cbind(xv, yv)[chull(xv, yv), , drop = FALSE]

  for (k in seq_len(nrow(ep))) {
    in_bag <- if (!is.null(hull_bag)) {
      point_in_poly(ep$px[k], ep$py[k], hull_bag)
    } else NA
    in_loop <- if (!is.null(hull_loop)) {
      point_in_poly(ep$px[k], ep$py[k], hull_loop)
    } else NA
    in_ch <- point_in_poly(ep$px[k], ep$py[k], ch)

    coverage_rows[[paste0(pred$tag, "_", k)]] <- data.frame(
      Predictor_Set = pred$label_en, Point = ep$Point[k],
      X = round(ep$px[k], 3), Y = round(ep$py[k], 3),
      Inside_Bag = in_bag, Inside_Loop = in_loop,
      Inside_ConvexHull = in_ch,
      stringsAsFactors = FALSE
    )
    if (isFALSE(in_loop) || (is.na(in_loop) && !in_ch)) {
      cat("[WARN]", pred$label_en,
          "- 求值点在数据支持域外:", ep$Point[k], "\n")
    }
  }

  # 绘图 (bagplot; 失败则凸包回退)
  ok_plot <- tryCatch({
    aplpack::bagplot(
      xv, yv,
      xlab = paste0("Parent ", pred$label_en, " (X, centered)"),
      ylab = paste0("Student ", pred$label_en, " (Y, centered)"),
      main = paste0(pred$label_en, ": data coverage"),
      xlim = pred$plot_lim, ylim = pred$plot_lim, cex = 0.6
    )
    TRUE
  }, error = function(e) FALSE)
  if (!ok_plot) {
    cat("[INFO]", pred$label_en, "- bagplot 失败, 使用凸包回退绘图。\n")
    plot(xv, yv, pch = 16, cex = 0.5, col = "grey55",
         xlab = paste0("Parent ", pred$label_en, " (X, centered)"),
         ylab = paste0("Student ", pred$label_en, " (Y, centered)"),
         main = paste0(pred$label_en, ": data coverage (convex hull)"),
         xlim = pred$plot_lim, ylim = pred$plot_lim)
    polygon(ch, border = "steelblue")
  }
  abline(0,  1, lty = 2, col = "grey30")
  abline(0, -1, lty = 3, col = "grey30")
  points(ep$px, ep$py, pch = 4, cex = 1.8, lwd = 2, col = "red")
  text(ep$px, ep$py, labels = c("L+", "L-", "E", "D"),
       pos = 3, col = "red", cex = 0.8)
  legend("topleft",
         legend = c("LOC (X = Y)", "LOIC (X = -Y)",
                    "Evaluation points (+/-1 pooled SD)"),
         lty = c(2, 3, NA), pch = c(NA, NA, 4),
         col = c("grey30", "grey30", "red"), bty = "n", cex = 0.8)
}

dev.off()

congruence_pair_table <- do.call(rbind, pair_rows)
coverage_table        <- do.call(rbind, coverage_rows)
rownames(congruence_pair_table) <- NULL
rownames(coverage_table)        <- NULL
stopifnot(nrow(coverage_table) == 8)
cat("[CHECK OK] 每对预测变量的比例与覆盖检查完成。\n")

# ==== 5B. 14 个模型的 listwise 样本比例 (复刻 run_mediated_rsa 删除逻辑) ====
model_rows <- list()
for (pred in PREDICTOR_SPECS_PC) {
  for (med in MEDIATOR_SPECS) {
    m_t2_c   <- paste0(med$m_t2, "_c")
    all_vars <- unique(c(pred$x, pred$y, pred$x2, pred$xy, pred$y2,
                         med$m_t3, "burnout_T4", CTRL_BASE, m_t2_c))
    d_model <- dat_analysis %>%
      select(all_of(all_vars)) %>%
      na.omit()

    # Shanock 口径的 z 在各模型子样本内计算 (自描述);
    # pooled 口径使用全局 SD_pooled 常数 (与主分析求值点一致)。
    ps <- prop3(classify_shanock(d_model[[pred$x]], d_model[[pred$y]]))
    pp <- prop3(classify_pooled(d_model[[pred$x]], d_model[[pred$y]],
                                pred$sd_pooled))

    model_rows[[paste0(pred$tag, "__", med$m_t3)]] <- data.frame(
      Predictor_Set = pred$label_en, Mediator = med$label_en,
      N = nrow(d_model),
      Shanock_Student_higher = ps[1], Shanock_Congruent = ps[2],
      Shanock_Parent_higher = ps[3],
      PooledSD_Student_higher = pp[1], PooledSD_Congruent = pp[2],
      PooledSD_Parent_higher = pp[3],
      Below_10pct_Shanock = below10_flag(ps),
      stringsAsFactors = FALSE
    )
    if (nzchar(below10_flag(ps))) {
      cat("[WARN] 模型", pred$label_en, "->", med$label_en,
          "- Shanock 口径下占比 <10% 的类别:", below10_flag(ps), "\n")
    }
  }
}
congruence_model_table <- do.call(rbind, model_rows)
rownames(congruence_model_table) <- NULL

stopifnot(nrow(congruence_model_table) == 14)
shanock_cols <- c("Shanock_Student_higher", "Shanock_Congruent",
                  "Shanock_Parent_higher")
pooled_cols  <- c("PooledSD_Student_higher", "PooledSD_Congruent",
                  "PooledSD_Parent_higher")
stopifnot(all(abs(rowSums(congruence_model_table[, shanock_cols]) - 100) < 0.5))
stopifnot(all(abs(rowSums(congruence_model_table[, pooled_cols])  - 100) < 0.5))
for (pred in PREDICTOR_SPECS_PC) {
  n_pair  <- congruence_pair_table$N[
    congruence_pair_table$Predictor_Set == pred$label_en]
  n_model <- congruence_model_table$N[
    congruence_model_table$Predictor_Set == pred$label_en]
  stopifnot(all(n_model <= n_pair))
}
cat("[CHECK OK] 14 个模型的一致/不一致比例已计算, N 范围:",
    min(congruence_model_table$N), "-", max(congruence_model_table$N), "\n")

# ==== 6. VIF (a 路径 x14, b 路径 x14, 总效应 x2) ====
vif_rows <- list()
add_vif <- function(model, pred_lab, model_type, med_lab, n) {
  v <- car::vif(model)
  data.frame(
    Predictor_Set = pred_lab, Model_Type = model_type,
    Mediator = med_lab, N = n,
    Term = names(v), VIF = round(as.numeric(v), 3),
    Flag = ifelse(v > 10, ">10", ifelse(v > 5, ">5", "")),
    stringsAsFactors = FALSE
  )
}

for (pred in PREDICTOR_SPECS_PC) {
  poly5 <- c(pred$x, pred$y, pred$x2, pred$xy, pred$y2)

  # 总效应模型 (burnout_T4 ~ poly5 + CTRL_BASE), 在其自身完整样本上
  tot_terms <- c(poly5, CTRL_BASE)
  d_tot <- dat_analysis %>%
    select(all_of(c("burnout_T4", tot_terms))) %>%
    na.omit()
  m_tot <- lm(reformulate(tot_terms, response = "burnout_T4"), data = d_tot)
  vif_rows[[paste0(pred$tag, "_total")]] <-
    add_vif(m_tot, pred$label_en, "total", NA, nrow(d_tot))

  # a / b 路径: 与主分析相同, 在各模型的 listwise 联合样本上拟合
  for (med in MEDIATOR_SPECS) {
    m_t2_c   <- paste0(med$m_t2, "_c")
    all_vars <- unique(c(poly5, med$m_t3, "burnout_T4", CTRL_BASE, m_t2_c))
    d_model  <- dat_analysis %>%
      select(all_of(all_vars)) %>%
      na.omit()

    m_a <- lm(reformulate(c(poly5, CTRL_BASE, m_t2_c),
                          response = med$m_t3), data = d_model)
    m_b <- lm(reformulate(c(poly5, med$m_t3, CTRL_BASE),
                          response = "burnout_T4"), data = d_model)

    vif_rows[[paste0(pred$tag, "_a_", med$m_t3)]] <-
      add_vif(m_a, pred$label_en, "a_path", med$label_en, nrow(d_model))
    vif_rows[[paste0(pred$tag, "_b_", med$m_t3)]] <-
      add_vif(m_b, pred$label_en, "b_path", med$label_en, nrow(d_model))
  }
}
vif_table <- do.call(rbind, vif_rows)
rownames(vif_table) <- NULL

stopifnot(all(is.finite(vif_table$VIF)), all(vif_table$VIF > 0))
for (mt in unique(vif_table$Model_Type)) {
  cat("[INFO] VIF 最大值 (", mt, "):",
      max(vif_table$VIF[vif_table$Model_Type == mt]), "\n")
}
if (any(vif_table$VIF > 10)) {
  cat("[WARN] 存在 VIF > 10 的项, 见 VIF sheet 的 Flag 列。\n")
}
cat("[CHECK OK] VIF 计算完成, 共", nrow(vif_table), "行。\n")

# ==== 7. 流失分析 ====

# 7.1 序贯流程表 (按主脚本筛选条件顺序)
n0 <- nrow(dat)
g1 <- dat %>% filter(has_T2_parent == 1)
g2 <- g1 %>% filter(has_T3 == 1)
g3 <- g2 %>% filter(has_T4 == 1)
g4 <- g3 %>% filter(caregiver_match_T2 == 1)
attrition_flow <- data.frame(
  Step = c("Matched file (total rows)",
           "+ has_T2_parent == 1",
           "+ has_T3 == 1",
           "+ has_T4 == 1",
           "+ caregiver_match_T2 == 1 (analysis sample)"),
  N = c(n0, nrow(g1), nrow(g2), nrow(g3), nrow(g4))
)
attrition_flow$N_dropped <- c(NA, -diff(attrition_flow$N))
stopifnot(nrow(g4) == nrow(dat_analysis))
cat("[CHECK OK] 流程表末行 N =", nrow(g4), "与分析样本一致。\n")

# 7.2 四个标志的组合频数
flag_combo <- dat %>%
  count(has_T2_parent, has_T3, has_T4, caregiver_match_T2, name = "N") %>%
  arrange(desc(N)) %>%
  as.data.frame()

# 7.3 纳入 vs 排除比较 (仅 T1/T2 变量; Welch t + Cohen d)
# 注: 家长报告变量在 has_T2_parent == 0 的排除个案中系统缺失,
#     其"排除组"实际为有家长数据但因 T3/T4/caregiver_match 被排除者,
#     以 N_excluded 列体现。
dat$included_analysis <- as.integer(
  !is.na(dat$has_T2_parent) & dat$has_T2_parent == 1 &
  !is.na(dat$has_T3)        & dat$has_T3 == 1 &
  !is.na(dat$has_T4)        & dat$has_T4 == 1 &
  !is.na(dat$caregiver_match_T2) & dat$caregiver_match_T2 == 1
)
stopifnot(sum(dat$included_analysis) == nrow(dat_analysis))

cohens_d_2g <- function(x1, x0) {
  n1 <- length(x1); n0 <- length(x0)
  if (n1 < 2 || n0 < 2) return(NA_real_)
  sp <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  (mean(x1) - mean(x0)) / sp
}

ATTRITION_VARS <- c(
  "parent_SES_T2", "burnout_T2",
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  MEDIATOR_T2_VARS
)

comp_rows <- list()
for (v in ATTRITION_VARS) {
  x1 <- as.numeric(dat[[v]][dat$included_analysis == 1])
  x0 <- as.numeric(dat[[v]][dat$included_analysis == 0])
  x1 <- x1[!is.na(x1)]
  x0 <- x0[!is.na(x0)]
  p <- if (length(x1) >= 2 && length(x0) >= 2) {
    tryCatch(t.test(x1, x0)$p.value, error = function(e) NA_real_)
  } else NA_real_
  comp_rows[[v]] <- data.frame(
    Variable = v,
    N_included = length(x1),
    Mean_included = round(mean(x1), 3),
    SD_included = round(stats::sd(x1), 3),
    N_excluded = length(x0),
    Mean_excluded = ifelse(length(x0) > 0, round(mean(x0), 3), NA),
    SD_excluded  = ifelse(length(x0) > 1, round(stats::sd(x0), 3), NA),
    Cohens_d = round(cohens_d_2g(x1, x0), 3),
    Welch_p  = ifelse(is.na(p), NA, round(p, 4)),
    stringsAsFactors = FALSE
  )
}
attrition_compare <- do.call(rbind, comp_rows)
rownames(attrition_compare) <- NULL

# 7.4 性别 x 纳入 卡方
sex_tab <- table(Included = dat$included_analysis,
                 Sex = dat$sex_T1, useNA = "no")
sex_chi <- tryCatch(suppressWarnings(chisq.test(sex_tab)),
                    error = function(e) NULL)
attrition_sex <- as.data.frame(sex_tab)
attrition_sex$Chisq_p <- ifelse(is.null(sex_chi), NA,
                                round(sex_chi$p.value, 4))

# 7.5 分析样本内缺失率
MISS_VARS <- c(
  "student_overparenting_T2", "parent_overparenting_T2",
  "student_autonomy_support_T2", "parent_autonomy_support_T2",
  "burnout_T2", "burnout_T3", "burnout_T4",
  "sex_T1", "parent_SES_T2",
  MEDIATOR_T2_VARS, sub("_T2$", "_T3", MEDIATOR_T2_VARS)
)
missingness <- data.frame(
  Variable  = MISS_VARS,
  N_valid   = sapply(MISS_VARS,
                     function(v) sum(!is.na(dat_analysis[[v]]))),
  N_missing = sapply(MISS_VARS,
                     function(v) sum(is.na(dat_analysis[[v]])))
)
missingness$Pct_missing <- round(
  100 * missingness$N_missing / nrow(dat_analysis), 2)
rownames(missingness) <- NULL
cat("[CHECK OK] 流失分析与缺失率计算完成。\n")

# ==== 8. 导出 ====
write_xlsx(list(
  Congruence_Pair   = congruence_pair_table,
  Congruence_Model  = congruence_model_table,
  Coverage          = coverage_table,
  VIF               = vif_table,
  Attrition_Flow    = attrition_flow,
  Attrition_Flags   = flag_combo,
  Attrition_Compare = attrition_compare,
  Attrition_Sex     = attrition_sex,
  Missingness       = missingness
), path = "results_precheck_congruence_vif_attrition.xlsx")

stopifnot(file.exists("results_precheck_congruence_vif_attrition.xlsx"),
          file.exists("precheck_bagplots.pdf"))
cat("[CHECK OK] 输出文件已生成:\n")
cat("  - results_precheck_congruence_vif_attrition.xlsx (9 sheets)\n")
cat("  - precheck_bagplots.pdf (2 页)\n")
cat("========== 前置检验脚本运行完毕 ==========\n")
