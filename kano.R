suppressPackageStartupMessages(library(dplyr))

plot_kano <- function(nodes, lf_edges, palette = NULL) {
  nd <- nodes %>% transmute(name=as.character(name), value=as.numeric(value), carac=as.character(carac))
  outw <- lf_edges %>% group_by(Leader) %>% summarise(outW=sum(as.numeric(WCD), na.rm=TRUE), .groups="drop") %>%
    rename(name=Leader)
  inw <- lf_edges %>% group_by(follower) %>% summarise(inW=sum(as.numeric(WCD), na.rm=TRUE), .groups="drop") %>%
    rename(name=follower)

  df <- nd %>% left_join(outw, by="name") %>% left_join(inw, by="name")
  .data$outW[is.na(.data$outW)] <- 0
  .data$inW[is.na(.data$inW)] <- 0
  .data$performance <- .data$outW - .data$inW

  x <- .data$value
  y <- .data$performance

  if (is.null(palette)) palette <- full_color_set(unique(.data$carac))
  cols <- palette[.data$carac]

  plot(x, y, pch=19, cex=1.2, col=cols,
       xlab="Importance (node strength)", ylab="Performance (outW - inW)",
       main="IPA-Kano (colors by cluster)")
  text(x, y, labels=.data$name, pos=3, cex=0.7)
  abline(v=median(x, na.rm=TRUE), lty=2)
  abline(h=median(y, na.rm=TRUE), lty=2)
}
