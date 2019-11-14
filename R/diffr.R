#' A version of setdiff with a "reverse order" option and a "cbind" version
#' that combines the "regular" and the "reverse" order into a data.frame.
#'
#' @param x A vector
#' @param y A vector
#' @param order One of "xy", "yx", or "df".
#' @return a vector of items present in X (or Y) but not in Y (or X)
#' @export
diffr <- function (x, y, order = "xy") {
  x <- as.vector(x)
  y <- as.vector(y)
  if(order != "xy" & order != "yx" & order != "df"){
    stop("order must be one of 'xy', 'yx', or 'df'.")
  } else if (order == "xy"){
    unique(if (length(x) || length(y))
      x[match(x, y, 0L) == 0L]
      else x)
  } else if (order == "yx"){
    unique(if (length(x) || length(y))
      y[match(y, x, 0L) == 0L]
      else y)
  } else if (order == "df"){
    cbind(sort(unique(if (length(x) || length(y))
      x[match(x, y, 0L) == 0L]
      else x)),
      sort(unique(if (length(x) || length(y))
        y[match(y, x, 0L) == 0L]
        else y)))
  }
}

#' Better theme_map() for ggplot2.
#'
#' @importFrom ggplot2 theme theme_minimal element_text element_blank 
#' @importFrom ggplot2 element_line element_rect
#' @param ... other options to ggplot2::theme()
#' @export
theme_map <- function(...) {
theme_minimal() +
  theme(text = element_text(family = "Brandon Grotesque Medium",
                            color = "#22211d", size = 13),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2"),
        panel.background = element_rect(fill = "#f5f5f2"),
        legend.background = element_rect(fill = "#f5f5f2"),
        strip.background = element_rect(fill = "#f5f5f2"),
        panel.border = element_blank(),
        rect = element_rect(linetype = 0, colour = NA),
        legend.key = element_rect(fill = "#f5f5f2"),
        ...)
}

#' A function that extends ggplot2::annotation_custom by automatically
#' picking a textGrob, and allowing the user to pass the graphical parameters
#' of the grob into the function call itself.
#' 
#' @importFrom grid textGrob gpar
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::annotation_custom
#' @param label a string.
#' @param ... optional, passed to grid::gpar.
#' 
#' @export
annotation_custom_text <- function (label, xmin = -Inf, xmax = Inf, 
                                    ymin = -Inf, ymax = Inf, ...) {
  grob <- grid::textGrob(label, gp = grid::gpar(...))
  layer(data = data.frame(x = NA), stat = StatIdentity, 
        position = PositionIdentity, geom = GeomCustomAnn, 
        inherit.aes = FALSE, 
        params = list(grob = grob, xmin = xmin, xmax = xmax, 
                      ymin = ymin, ymax = ymax))
  cat(paste("Don't forget to set plot.margin in theme() and clip = 'off'",
            "in coord_cartesian()!"))
}



#' A function to automatically remove temporary objects from your .GlobalEnv.
#'
#' @param pattern a regular expression passed to rm
#' @export
delete_this <- function(pattern = 'nephew'){
  rm(list = ls(pattern = pattern, name = '.GlobalEnv'), pos = 1)
}

#' A "prettified" object.size that returns sizes formatted in Mb. The point
#' here is exclusively, so far, to avoid having to type
#' 'format(object.size(x), 'Mb') so much.
#'
#' @importFrom utils object.size
#' @param object an R object.
#' @export
object_size <- function(object){
  return(format(object.size(object), 'Mb'))
}

#' A function that prints a named vector with the object_sizes of the objects
#' in the .GlobalEnv.
#' @export
global_size <- function(){
  sapply(sapply(ls(.GlobalEnv), as.symbol), function(x)object_size(eval(x)))
}

#' A version of prop.table that takes any R object that the 'table' function
#' will accept. This, like object_size, is a convenience function so that you
#' don't have to always type prop.table(table(object)). This function allows you
#' to specify the margin of the table as the LAST unnamed argument, meaning that
#' the function works if every argument you pass *after* margin is named. Of
#' course, you can explicitly pass 'margin = n' and then everything should work
#' as expected. For now, if you pass margin as an unnamed argument, it
#' cannot be a vector (unlike in prop.table).
#'
#' @param ... one or more objects which can be parsed by the base table function
#' @param margin index to generate margin for
#' @importFrom utils head
#' @importFrom utils tail
#' @inheritParams base::table
#' @export
prop_table <- function (..., margin = 0,
                        exclude = if (useNA == "no") c(NA, NaN),
                        useNA = c("no", "ifany", "always"),
                        dnn = list.names(...), deparse.level = 1) {
  args <- list(...)
  argnames <- as.character(substitute(list(...)))
  if (tail(argnames, 1) %in% c(1:3)) 
    margin <- tail(args, 1)[[1L]]
  list.names <- function(...) {
    l <- as.list(substitute(list(...)[-1]))[-1L]
    if (length(l) == 3) l <- l[-3]
    nm <- names(l)
    fixup <- if (is.null(nm))
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x)
      switch(deparse.level + 1, "", if (is.symbol(x)) as.character(x) else "",
             deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm))
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  miss.use <- missing(useNA)
  miss.exc <- missing(exclude)
  useNA <- if (miss.use && !miss.exc && !match(NA, exclude,
                                               nomatch = 0L))
    "ifany"
  else match.arg(useNA)
  doNA <- useNA != "no"
  if (!miss.use && !miss.exc && doNA && match(NA, exclude,
                                              nomatch = 0L))
    warning(paste("'exclude' containing NA and 'useNA' != \"no\"'",
                  "are a bit contradictory"))
  if (!length(args))
    stop("nothing to tabulate")
  if (length(args) == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    if (length(dnn) != length(args))
      dnn <- if (!is.null(argn <- names(args)))
        argn
    else paste(dnn[1L], seq_along(args), sep = ".")
  }
  else if (tail(argnames, 1) %in% c(1:3)) {
    args <- head(args, -1)
  }
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (a in args) {
    if (is.null(lens))
      lens <- length(a)
    else if (length(a) != lens)
      stop("all arguments must have the same length")
    fact.a <- is.factor(a)
    if (doNA)
      aNA <- anyNA(a)
    if (!fact.a) {
      a0 <- a
      a <- factor(a, exclude = exclude)
    }
    add.na <- doNA
    if (add.na) {
      ifany <- (useNA == "ifany")
      anNAc <- anyNA(a)
      add.na <- if (!ifany || anNAc) {
        ll <- levels(a)
        if (add.ll <- !anyNA(ll)) {
          ll <- c(ll, NA)
          TRUE
        }
        else if (!ifany && !anNAc)
          FALSE
        else TRUE
      }
      else FALSE
    }
    if (add.na)
      a <- factor(a, levels = ll, exclude = NULL)
    else ll <- levels(a)
    a <- as.integer(a)
    if (fact.a && !miss.exc) {
      ll <- ll[keep <- which(match(ll, exclude, nomatch = 0L) ==
                               0L)]
      a <- match(a, keep)
    }
    else if (!fact.a && add.na) {
      if (ifany && !aNA && add.ll) {
        ll <- ll[!is.na(ll)]
        is.na(a) <- match(a0, c(exclude, NA), nomatch = 0L) >
          0L
      }
      else {
        is.na(a) <- match(a0, exclude, nomatch = 0L) >
          0L
      }
    }
    nl <- length(ll)
    dims <- c(dims, nl)
    if (prod(dims) > .Machine$integer.max)
      stop("attempt to make a table with >= 2^31 elements")
    dn <- c(dn, list(ll))
    bin <- bin + pd * (a - 1L)
    pd <- pd * nl
  }
  if (length(args) > 1)
    names(dn) <- dnn
  bin <- bin[!is.na(bin)]
  if (length(bin))
    bin <- bin + 1L
  y <- array(tabulate(bin, pd), dims, dimnames = dn)
  class(y) <- "table"
  z <- if (length(margin) && margin > 0)
    sweep(y, margin, margin.table(y, margin), "/", check.margin = FALSE)
  else if (margin == 0) y/sum(y)
  else y/sum(y)
  z
}


#' A (largely untested) function that will take a named vector or a named list
#' and reverse elements/names: the names become the elements, and the elements
#' become the names. This is useful to me because I can never remember the
#' structure of named lists preferred by the custom.coef.map argument in texreg.
#'
#' @param veclist a named vector or list.
#' @param coerce coerce a list to a vector? silently ignored if veclist is a 
#' vector.
#' @export
reverse_names <- function(veclist, coerce = FALSE){
  if (is.null(names(veclist))){
    stop('This function only works with named vectors or lists.')
  }
  if (is.list(veclist) & !coerce){
    x <- as.list(names(veclist))
    names(x) <- veclist
    return(x)
  }
  else{
  x <- names(veclist)
  names(x) <- veclist
  return(x)
  }
}

#' A version of ggsave() meant to be piped to from a ggplot call or a ggplot
#' object.
#' 
#' @importFrom ggplot2 last_plot is.ggplot
#' @inheritParams ggplot2::ggsave
#' @export
save_gg <- function (plot = last_plot(), filename, device = NULL, path = NULL,
                     scale = 1, width = NA, height = NA,
                     units = c("in", "cm", "mm"),
                     dpi = 300, limitsize = TRUE, ...) {
  args <- lapply(substitute(list(...)), deparse)
  if (is.character(plot)){
    stop(paste('You passed a string as the first argument of this function.',
               'Are you using ggsave() syntax? This function is meant to be',
               'piped to from a ggplot() call, so the first argument is the',
               'plot itself, not the filename.'))
  }
  else if (!is.ggplot(plot)){
    stop('plot needs to be a "ggplot" class object.')
  }
  dpi <- ggplot2:::parse_dpi(dpi)
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units,
                            limitsize = limitsize)
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  old_dev <- grDevices::dev.cur()
  if (capabilities('cairo') & grepl('\\.png$', filename) &
      !('type' %in% names(args))){
    dev(filename = filename, width = dim[1], height = dim[2],
        type = 'cairo-png', ...)
  }
  else{
    dev(filename = filename, width = dim[1], height = dim[2],
        ...)
  }
  on.exit(utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }))
  ggplot2:::grid.draw.ggplot(plot)
  invisible()
}