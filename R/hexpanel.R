panel.hexloess <-
function(bin, w=NULL, span = 2/3, degree = 1, family = c("symmetric",
         "gaussian"), evaluation = 50, lwd = add.line$lwd, lty = add.line$lty,
         col, col.line = add.line$col, ...)
{
    add.line <- trellis.par.get("add.line")
   
     x <- bin@xcm
     y <- bin@ycm

     if(is.null(w))w <- bin@count
     control <- loess.control(...)
     notna <- !(is.na(x) | is.na(y))
     new.x <- seq(min(x[notna]), max(x[notna]), length = evaluation)
     family <- match.arg(family)

     fit <- stats::loess(y~x, weights=w, span=span, degree=degree, normalize = FALSE,
                                family=family, control=control)
     z<-predict(fit,new.x)
     if (length(x) > 0) {
          if (!missing(col) && missing(col.line)) col.line <- col
          add.line <- trellis.par.get("add.line")
          panel.lines(new.x, z, col = col.line, lty = lty, lwd = lwd)
     }
}

panel.hexgrid <- function(h, border=grey(.85))
{
  hexGraphPaper(h,border=border)
}
