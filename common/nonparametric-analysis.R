
wants <- c('coin', 'reshape', 'dplyr', 'r2excel', 'readr', 'rcompanion', 'FSA')
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

library(coin)
library(dplyr)
library(readr)
library(reshape)
library(r2excel)
library(rcomp)
library(rcompanion)
library(FSA)

## plot function of wilcox_analysiss
plot_wilcox.test <- function(
  wt, title="", sub = NULL, ylab = NULL, notch = T
  , inv.col = F, draw.conf.int = T, ylim = NULL, levels = NULL) {
  
  x <- factor(wt$data$x)
  y <- wt$data$y
  
  pch1=16; pch2=17
  pcol1=10; pcol2=4
  pcol = c("white", "lightgrey")
  if (inv.col) {
    pch1 = 17; pch2 = 16
    pcol1 = 4; pcol2 = 10
    pcol = c("lightgrey", "white")
  }
  if (is.null(levels)) {
    x <- factor(x)
  } else x <- factor(x, levels = levels)
  
  bp <- boxplot(y ~ x, boxwex=0.2, notch=notch, col=pcol, ylab=ylab, ylim = ylim)
  title(title, sub = sub)
  
  # drawing data as points
  stripchart(y[x==levels(x)[1]], col=8, pch=pch1, add=T, at=0.7, cex=.7, method="jitter", vertical=T)
  stripchart(y[x==levels(x)[2]], col=8, pch=pch2, add=T, at=1.7, cex=.7, method="jitter", vertical=T)
  
  # drawing line wilcox conf.interval
  if (draw.conf.int) {
    wt <- tryCatch(wilcox.test(y[x==levels(x)[1]], conf.int=T), error = function(e) NULL)
    if (!is.null(wt)) {
      points(c(0.7,0.7,0.7), c(wt$conf.int, wt$estimate), pch="-", col=pcol1, cex=c(.9,.9,1.5))
      lines(c(0.7,0.7), wt$conf.int, col=pcol1)
    }
    
    wt <- tryCatch(wilcox.test(y[x==levels(x)[2]], conf.int=T), error = function(e) NULL)
    if (!is.null(wt)) {
      points(c(1.7,1.7,1.7), c(wt$conf.int, wt$estimate), pch="-", col=pcol2, cex=c(.9,.9,1.5))
      lines(c(1.7,1.7), wt$conf.int, col=pcol2)
    }
  }
}

## Function to get wilcox_module
get_wilcox_mod <- function(x, y,  alternative = "two.sided") {
  
  library(coin)
  
  x <- factor(x)
  sdata <- data.frame(x=x, y=y, r=rank(y))
  
  # wilcoxon values
  wt <- wilcox.test(y ~ x, alternative = alternative, conf.int = FALSE)
  U <- wt$statistic
  wt <- wilcox_test(y ~ x, distribution="exact", conf.int = FALSE, alternative = alternative)
  Z <- as.numeric(statistic(wt))
  pvalue <- pvalue(wt)
  r <- abs(Z/sqrt(length(x)))
  magnitude <- 'none'
  if (r >= 0.1 && r < 0.3) magnitude <- 'small'
  if (r >= 0.3 && r < 0.5) magnitude <- 'medium'
  if (r >= 0.5) magnitude <- 'large'
  
  result <- data.frame(
    "Group" = c(levels(x)[1], levels(x)[2])
    , "N" = c(length(x[x==levels(x)[1]]), length(x[x==levels(x)[2]]))
    , "Median" = c(median(sdata$y[sdata$x == levels(x)[1]]), median(sdata$y[sdata$x == levels(x)[2]]))
    , "Mean Ranks" = c(mean(sdata$r[sdata$x == levels(x)[1]]), mean(sdata$r[sdata$x == levels(x)[2]]))
    , "Sum Ranks" = c(sum(sdata$r[sdata$x == levels(x)[1]]), sum(sdata$r[sdata$x == levels(x)[2]]))
    , "U" = c(U, U)
    , "Z" = c(Z, Z)
    , "p-value" = c(pvalue, pvalue)
    , "r" = c(r, r)
    , "magnitude" = c(magnitude, magnitude)
  )
  
  return(list(data = sdata, result = result))
}

## Function to get wilcox test modules 
get_wilcox_mods <- function(dat, dv, iv, between) {
  
  dat <- dat[complete.cases(dat[dv]),]
  
  result <- list()
  columns <- unique(c(iv, between[!between %in% iv]))
  
  for (m in 1:length(columns)) {
    comb_columns <- combn(columns, m, simplify = T)
    for (i in 1:ncol(comb_columns)) {
      selected_columns  <- comb_columns[,i]
      if (selected_columns[[1]] != iv) next
      cname <- paste0(selected_columns, collapse = ':')
      factors <- factor(apply(dat[selected_columns], 1, paste, collapse='.'))
      level_pairs <- combn(levels(factors), 2)
      
      mods <- list()
      for (j in 1:ncol(level_pairs)) {
        level_pair <- level_pairs[,j]
        
        rdat <- dat[factors %in% level_pair,]
        y <- rdat[[dv]]
        x <- factors[factors %in% level_pair]
        
        wt_1 <- get_wilcox_mod(x, y, alternative = 'less')
        wt_2 <- get_wilcox_mod(x, y, alternative = 'greater')
        wt_3 <- get_wilcox_mod(x, y, alternative = 'two.sided')
        
        mods[[paste0(level_pair, collapse = ':')]] <- list(dat = rdat, less = wt_1, greater = wt_2, two.sided = wt_3)
      }
      result[[cname]] <- mods
    }
  }
  return(result)
}


## Function to perform the parametric test
# TODO: non-parametric test 
do_nonparametric_test <- function(dat, wid, dv, iv, between, observed = NULL
                                  , within = NULL, p_limit = 0.05, completed = F) {
  
  library(rcompanion)
  
  wdat <- dat
  columns <- base::unique(c(iv, between, observed))
  for (cname in columns) wdat[[cname]] <- factor(wdat[[cname]])
  dv_tmp <- gsub('\\W', '', dv) # temporal name for dependent variable
  wdat[[dv_tmp]] <- wdat[[dv]]
  
  mod <- NULL
  post_hoc <- NULL
  formula_str <- paste(dv_tmp, "~", paste(columns, collapse = "+"))
  if (length(columns) == 1) {
    # get_Scheirer Kruskal
    mod <- kruskal.test(as.formula(formula_str), data = wdat)
    mod_df <- as.data.frame(list(`chi-square` = mod$statistic
                            , df = mod$parameter
                            , `p.value` = mod$p.value))
  } else if (length(columns) == 2) {
    # get_Scheirer Ray Hare test module
    mod <- scheirerRayHare(formula = as.formula(formula_str), data = wdat)
    mod_df <- as.data.frame(mod)
  }
  
  # Post.hoc Dunn test
  dunn_mods <- list()
  columns <- unique(c(iv, columns[!columns %in% iv]))
  for (m in 1:length(columns)) {
    comb_columns <- combn(columns, m, simplify = T)
    
    for (i in 1:ncol(comb_columns)) {
      selected_columns  <- comb_columns[,i]
      #if (selected_columns[[1]] != iv) next
      cname <- paste0(selected_columns, collapse = ':')
      
      factors <- factor(apply(wdat[selected_columns], 1, paste, collapse='.'))
      dunn_mods[[cname]] <- dunnTest(x = wdat[[dv_tmp]], g = factors, method = "bonferroni")
    }
  }
  post_hoc <- list(mods = dunn_mods)
  
  if (dv_tmp != dv) {
    wdat <- wdat[ , !(names(wdat) %in% c(dv_tmp))]
  }
  set_wt_mods <- get_wilcox_mods(dat, dv = dv, iv = iv, between = between)
  
  return(list(data = wdat, mod = mod, mod.df = mod_df, formula.str = formula_str
              , post.hoc = post_hoc, wilcox.pairs = set_wt_mods))
}

##############################################################

## function to write wilcoxon test in sheet
write_wts_in_wb <- function(wt_mods, wb, iv, i, title = "", ylab = "Score", ylim = NULL, levels = NULL) {
  library(r2excel) 
  
  wt_mod <- wt_mods[[i]]
  sheet <- xlsx::createSheet(wb, sheetName = paste0(sub(':', '_', iv),"_", i))
  xlsx.addHeader(wb, sheet, paste0("Wilcoxon Analysis for ", title, " in ", iv, " between ", names(wt_mods)[[i]], collapse = ''), startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Wilcoxon test results", level = 2, startCol = 1)
  xlsx.addHeader(wb, sheet, "Alternative hypothesis: less", level = 3, startCol = 1)
  xlsx.addTable(wb, sheet, wt_mod$less$result, startCol = 1, row.names = F)
  xlsx.addHeader(wb, sheet, "Alternative hypothesis: greater", level = 3, startCol = 1)
  xlsx.addTable(wb, sheet, wt_mod$greater$result, startCol = 1, row.names = F)
  xlsx.addHeader(wb, sheet, "Alternative hypothesis: two.sided", level = 3, startCol = 1)
  xlsx.addTable(wb, sheet, wt_mod$two.sided$result, startCol = 1, row.names = F)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Box plots for wilcoxon tests", level = 2, startCol = 1)
  plotWT <- function() {
    ##
    pair_names <- strsplit(names(wt_mods)[[i]], ':')[[1]]
    ## sorting levels
    pair_levels <- NULL
    if (!is.null(levels)) {
      pair_levels <- c()
      for (lvl in levels) {
        pair_levels <- c(pair_levels, pair_names[grepl(lvl, pair_names)])
      }
    }
    
    plot_wilcox.test(wt_mod$two.sided, title = title, ylab = ylab, ylim = ylim, levels = pair_levels)
  }
  xlsx.addPlot(wb, sheet, plotWT, width = 640, height = 640, startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Wilcoxon data", level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, wt_mod$dat, startCol = 1, row.names = F)
}

## Function to write summary of set wt mods
write_wts_summary_in_wb <- function(set_wt_mods, wb, title = "") {
  
  library(r2excel) 
  
  sheet <- xlsx::createSheet(wb, sheetName = "Summary")
  xlsx.addHeader(wb, sheet, paste0("Summary of Wilcoxon Analysis for ", title, collapse = ''), startCol = 1)
  
  for (iv in names(set_wt_mods)) {
    wt_mods <- set_wt_mods[[iv]]
    for (i in 1:length(wt_mods)) {
      wt_mod <- wt_mods[[i]]
      
      if (max(wt_mod$less$result$p.value) <= 0.05) {
        xlsx.addLineBreak(sheet, 2)
        xlsx.addHeader(wb, sheet, paste0("Wilcoxon test results for ", iv, " - Alternative hypothesis: less"), level = 2, startCol = 1)
        xlsx.addTable(wb, sheet, wt_mod$less$result, startCol = 1, row.names = F)
      }
      if (max(wt_mod$greater$result$p.value) <= 0.05) {
        xlsx.addLineBreak(sheet, 2)
        xlsx.addHeader(wb, sheet, paste0("Wilcoxon test results for ", iv, " - Alternative hypothesis: greater"), level = 2, startCol = 1)
        xlsx.addTable(wb, sheet, wt_mod$greater$result, startCol = 1, row.names = F)
      }
    }
  }
}

## Function to write plots of wilcoxon test
write_wilcoxon_plots <- function(
  set_wt_mods, ylab, title, path, override = T, ylim = NULL, levels = NULL) {
  for (iv in names(set_wt_mods)) {
    wt_mods <- set_wt_mods[[iv]]
    for (i in 1:length(wt_mods)) {
      wt_mod <- wt_mods[[i]]
      
      filename <- paste0(iv, '_', names(wt_mods)[[i]], ".png")
      filename <- gsub(':', '.', gsub('/', '', filename))
      filename <- paste0(path, filename)
      
      
      ##
      pair_names <- strsplit(names(wt_mods)[[i]], ':')[[1]]
      ## sorting levels
      pair_levels <- NULL
      if (!is.null(levels)) {
        pair_levels <- c()
        for (lvl in levels) {
          pair_levels <- c(pair_levels, pair_names[grepl(lvl, pair_names)])
        }
      }
      
      if (!file.exists(filename) || override) {
        png(filename = filename, width = 640, height = 640)
        plot_wilcox.test(wt_mod$two.sided, title = title, ylab = ylab, ylim = ylim, levels = pair_levels)
        dev.off()
      }
      
    }
  }
}

# Function to write 
write_plots_for_nonparametric_test <- function(
  n_result, ylab, title, path, override = T, ylim = NULL, levels = NULL) {
  write_wilcoxon_plots(
    n_result$wilcox.pairs, ylab = ylab, title = title, path = path
    , override = override, ylim = ylim, levels = levels)
}

write_sch_summary_in_wb <- function(n_result, wb, title = "") {
  
  library(r2excel)
  
  sheet <- xlsx::createSheet(wb, sheetName = "NonParametricTest")
  xlsx.addHeader(wb, sheet, paste0("Summary of ", n_result$mod$method, " for ", title), startCol = 1)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, paste0(n_result$mod$method," Table: ", n_result$formula.str), level = 2, startCol = 1)
  xlsx.addTable(wb, sheet, as.data.frame(n_result$mod.df), startCol = 1, row.names = T)
  
  xlsx.addLineBreak(sheet, 2)
  xlsx.addHeader(wb, sheet, "Post-hoc Multiple test", level = 2, startCol = 1)
  for (cname in names(n_result$post.hoc$mods)) {
    mod <- n_result$post.hoc$mods[[cname]]
    xlsx.addLineBreak(sheet, 1)
    xlsx.addHeader(wb, sheet, paste0("Dunn's Kruskal-Wallis  (", mod$method, ") for ", cname), level = 3, startCol = 1)
    xlsx.addTable(wb, sheet, as.data.frame(mod$res), startCol = 1, row.names = F)  
  }
}

## Function to write anova analysis report
write_nonparametric_test_report <- function(n_result, filename, title = "", ylab = "Score", override = T, data = NULL, ylim = NULL, levels = NULL) {
  library(r2excel)
  if (!file.exists(filename) || override) {
    wb <- createWorkbook(type="xlsx")
    write_sch_summary_in_wb(n_result, wb, title)
    
    # write pair_wilcoxon
    write_wts_summary_in_wb(n_result$wilcox.pairs, wb, title = title)
    for (iv in names(n_result$wilcox.pairs)) {
      wt_mods <- n_result$wilcox.pairs[[iv]]
      for (i in 1:length(wt_mods)) {
        write_wts_in_wb(wt_mods, wb, iv, i, title = title, ylab = ylab, ylim = ylim, levels = levels)
      }
    }
    if (is.null(data)) data <- n_result$data
    
    sheet <- xlsx::createSheet(wb, sheetName = "data")
    xlsx.addTable(wb, sheet, data, startCol = 1, row.names = F)
    
    ##
    saveWorkbook(wb, filename)
  }
}
