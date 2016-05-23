#' Using mixed methods to visualize a correlation matrix.
#'
#' @param corr Matrix, the correlation matrix to visualize.
#' @param lower Character, the visualization method for the lower triangular
#'   correlation matrix.
#' @param upper Character, the visualization method for the upper triangular
#'   correlation matrix.
#' @param tl.pos Character, \code{"lt"}, \code{"d"} or \code{"n"}, giving
#'   position of text labels, \code{"lt"} means left and top,  \code{"d"} means
#'   diagonal. If \code{"n"},  add no textlabel.
#' @param diag Character, for specifying the glyph on the principal diagonal. It
#'   is one of \code{"n"} (default,  draw nothing), \code{"l"} (draw the glyphs
#'   of lower triangular) or \code{"u"} (draw the glyphs of upper triangular).
#' @param bg The background color.
#' @param addgrid.col See the \code{addgrid.col} parameter in the function
#'   \code{\link{corrplot}}
#' @param plotCI See the \code{plotCI} parameter in the function
#'   \code{\link{corrplot}}
#' @param \dots Additional arguments for corrplot's wrappers
#'
#' @author Taiyun Wei
#' @example vignettes/example-corrplot.mixed.R
#' @export
corrplot.pair <- function(
  corr1, corr2,
  lower = "circle",
  upper = "circle",
  col1 = NULL, col2 = NULL,
  order = c("original", "AOE", "FPC", "hclust", "alphabet"),
  order.guide = c("l", "u"),
  hclust.method = c("complete", "ward", "ward.D", "ward.D2", "single",
                    "average", "mcquitty", "median", "centroid"),
  tl.pos = c("d", "lt", "n"), tl.cex = 1,
  cl.offset = c(2, 0.5),
  diag = c("n", "l", "u"),
  bg = "white",
  addgrid.col = "grey",
  p.mat1 = NULL, sig.level1 = 0.05,
  p.mat2 = NULL, sig.level2 = 0.05,
  sig = c("pch", "p-value", "blank", "n"),
  insig = c("pch", "p-value", "blank", "n"),
  pch1 = 4, pch1.col = "black", pch1.cex = 3,
  pch2 = 4, pch2.col = "black", pch2.cex = 3,
  plotCI = c("n", "square", "circle", "rect"),
  ...)
{
  order <- match.arg(order)
  order.guide <- match.arg(order.guide)
  tl.pos <- match.arg(tl.pos)
  diag <- match.arg(diag)
  n <- nrow(corr1)
  if (length(sig) == 1 && length(insig) > 1) {
    insig <- "n"
  } else if (length(sig) > 1) {
    sig <- "n"
    insig <- match.arg(insig)
  }

  if (!identical(dim(corr1), dim(corr2))) {
    stop("corr1 and corr2 should have same dimentions")
  }

  if (is.null(col1)) {
    col1 <- colorRampPalette(c("#6800AD", "#8633BD", "#A466CD", "#C299DE",
                               "#E0CCEE", "#FFFFFF", "#E9EECB", "#D4DE98",
                               "#BECD65", "#A9BD32", "#94AD00"))(200)
  }
  if (is.null(col2)) {
    col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                               "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                               "#4393C3", "#2166AC", "#053061"))(200)
  }

  if (order != "original") {
    if (order.guide == "u") {
      ord <- corrMatOrder(corr1, order = order, hclust.method = hclust.method)
    } else if (order.guide == "l") {
      ord <- corrMatOrder(corr2, order = order, hclust.method = hclust.method)
    }
    corr1 <- corr1[ord, ord]
    corr2 <- corr2[ord, ord]
    p.mat1 <- p.mat1[ord, ord]
    p.mat2 <- p.mat2[ord, ord]
    order <- "original"
  }

  # fixes issue #21
  # some methods are not compatible with plotCI="rect"
  adjust_plotCI <- function(plotCI, method) {
    if (plotCI != "rect" || method %in% c("circle", "square")) {
      return(plotCI)
    }
    return("n")
  }

  plotCI_lower <- adjust_plotCI(plotCI, lower)
  plotCI_upper <- adjust_plotCI(plotCI, upper)

  corrplot(corr1, type = "upper", method = upper, diag = TRUE,
           col = col1, order = order,
           tl.pos = tl.pos, tl.cex = tl.cex,
           cl.offset = cl.offset, plotCI = plotCI_upper,
           p.mat = p.mat1, sig.level = sig.level1,
           sig = sig, insig = insig,
           pch = pch1, pch.col = pch1.col, pch.cex = pch1.cex, ...)

  corrplot(corr2, add = TRUE, type = "lower", method = lower,
           col = col2, order = order,
           tl.pos = "n", tl.cex = tl.cex,
           cl.pos = "r", plotCI = plotCI_lower,
           p.mat = p.mat2, sig.level = sig.level2,
           sig = sig, insig = insig,
           pch = pch2, pch.col = pch2.col, pch.cex = pch2.cex, ...)

  if (diag == "n" && tl.pos != "d") {
    symbols(1:n, n:1, add = TRUE, bg = bg, fg = addgrid.col,
            inches = FALSE, squares = rep(1, n))
  }

  # fixes issue #43
  # return value should be the same as in the corrplot function
  invisible(corr1)
}
