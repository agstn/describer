#' Create interactive histogram
#'
#' Code adapted from Hmisc package's spike histograms.
#'
#' @importFrom dplyr %>%
#' @import forcats
#' @importFrom Hmisc htmlSN plotlyParm
#' @import plotly
#'
#' @param x Variable from dataset in vector form.
#'
#' @return Plotly figure containing spike histogram (numeric) or traditional histogram (character).
#'
#' @examples
#' adsl <- safetyData::adam_adsl
#' create_hist(adsl$AGE)
#' create_hist(adsl$SITEID)
#'
#' @export
create_hist <- function(x) {
   UseMethod("create_hist")
}

#' @export
create_hist.character <- function(x){

   min_width = 50
   max_width = 250

   bins = 100

   distinct <- unique(x)
   distinct <- distinct[! is.na(distinct)]
   n_distinct <- length(distinct)
   xr       <- x
   mu <- Hmisc::markupSpecs$html
   fmt <- function(x) Hmisc::htmlSN(x, digits=5)

   dh <- dm <- dq1 <- dq2 <- dq3 <- dgmd <- dsd <- levs <- NULL

   mis   <- is.na(x)
   y <- -1
   miss <- sum(mis)
   i <- ! mis
   if(any(i)){
      u     <- x[i]
      ur    <- xr[i]
      tab   <- as.data.frame(table(ur))             # table of unique values & frequencies
      z     <- as.character(tab$ur)                 # unique values
      prop  <- tab$Freq / length(ur)                # proportion of obs for each unique value

      # df of unique values and proportion/freq/hovertext to display
      dh <- rbind(dh, data.frame(x=z, prop=prop, freq=tab$Freq,
                                 txt=paste0(fmt(z), '<br>', round(prop, 3),
                                            '<br>n=', tab$Freq),
                                 y=y))

      if (is.factor(x)){
         x_levs <- levels(x)
         dh <- dh %>% mutate(x = factor(x, levels = x_levs))
      } else {
         if (n_distinct > 30){
            dh <- dh %>%
               arrange(x) %>%
               mutate(x = factor(x))
         } else {
            dh <- dh %>%
               mutate(x = fct_reorder(x, prop) %>% fct_rev)
         }
      }
   }

   height <- Hmisc::plotlyParm$heightDotchart(1.2)

   dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

   width <- ifelse(n_distinct == 1, 17,
                   ifelse(n_distinct == 2, 35,
                          ifelse(n_distinct == 3, min_width,
                                 ifelse(n_distinct < max_width, scales::rescale(log(n_distinct),
                                                                                to = c(min_width, max_width),
                                                                                from = log(c(3, max_width))),
                                        max_width)) ) )


   # initiate plot
   p <- plotly::plot_ly(width = max_width+20,
                        height = 60)

   # add spike histogram lines
   p <- plotly::add_bars(p,
                         data = dh,
                         x = ~x,
                         y = ~ prop,
                         hoverinfo = 'text',
                         hovertext = ~ txt,
                         color = I('gray50'),
                         showlegend = FALSE)


   plotly::layout(p,
                  margin = list(l = 10, r = max_width+10-width, b = 0, t = 0, pad = 0),
                  xaxis = list(title='',
                               zeroline=FALSE,
                               visible = FALSE,
                               fixedrange = TRUE),
                  yaxis = list(title='',
                               visible = FALSE,
                               tickvals= -1,
                               ticktext = 1,
                               fixedrange = TRUE),
                #  plot_bgcolor="lightblue",
                  hoverlabel = list(font=list(size=12))) %>%
      config(displayModeBar = F) %>%
      partial_bundle()
}

#' @export
create_hist.factor <- create_hist.character

#' @export
create_hist.numeric <- function(x){

   p <- plotly::plot_ly(width  = 270,
                        height = 60)

   bins = 100
   distinct <- unique(x)
   distinct <- distinct[! is.na(distinct)]
   xmin     <- min(distinct)
   xr       <- x
   mu <- Hmisc::markupSpecs$html


   if (is.numeric(x)){
      ## Still do slight rounding if < bins distinct values because
      ## values extremely close to each other won't show otherwise
      if(length(distinct) > bins ||
         min(diff(sort(distinct))) < range(distinct) / (5 * bins)) {
         pret <- pretty(x, if(length(distinct) > bins) bins else 5 * bins)
         dist <- pret[2] - pret[1]
         r    <- range(pret)
         xr   <- r[1] + dist * round((x - r[1]) / dist)
      }
   }

   fmt <- function(x) Hmisc::htmlSN(x, digits=5)

   dh <- dm <- dq1 <- dq2 <- dq3 <- dgmd <- dsd <- levs <- NULL

   mis   <- is.na(x)
   y <- -1
   miss <- sum(mis)
   i <- ! mis
   if(any(i)){
      u     <- x[i]
      ur    <- xr[i]
      tab   <- as.data.frame(table(ur))             # table of unique values & frequencies
      if (is.numeric(x)){
         z     <- as.numeric(as.character(tab$ur))     # unique values
      } else if (lubridate::is.Date(x)){
         z     <- as.Date(as.character(tab$ur))     # unique values
      } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)){
         z     <- lubridate::as_datetime(as.character(tab$ur))     # unique values
      }
      prop  <- tab$Freq / length(ur)                # proportion of obs for each unique value

      # df of unique values and proportion/freq/hovertext to display
      dh <- rbind(dh, data.frame(x=z, prop=prop, freq=tab$Freq,
                                 txt=paste0(fmt(z), '<br>', round(prop, 3),
                                            '<br>n=', tab$Freq),
                                 y=y))

      # calculate mean
      dm <- rbind(dm, data.frame(Mean=mean(u), n=length(u), miss=miss, y=y))

      # calculate quartiles
      probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      if (is.numeric(x)){
         qu  <- quantile(u, probs)
      } else{
         qu  <- quantile(u, probs, type = 1)
      }
      nam <- paste0('Q', mu$sub(probs))
      txt <- paste0(nam, ':', fmt(qu))
      dq1 <- rbind(dq1, data.frame(Median=qu[3], txt=txt[3], y=y))               # hovertext for Median
      dq2 <- rbind(dq2, data.frame(quartiles=qu[c(2,4)], txt=txt[c(2,4)], y=y))  # hovertext for 25th & 75th
      dq3 <- rbind(dq3, data.frame(outer=qu[c(1,5)], txt=txt[c(1,5)], y=y))      # hovertext for 5th & 95th
   }

   height <- Hmisc::plotlyParm$heightDotchart(1.2)

   dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

   # add spike histogram lines
   p <- plotly::add_segments(p,
                             data=dh,
                             x = ~ x,       # segments to be drawn at data values on x-axis
                             xend = ~ x,
                             y = -1,        # segments to start at y=-1 and be length of % of values
                             yend = ~ -1 + prop,
                             text = ~ txt,       # hovertext to display for data values
                             hoverinfo = 'text',
                             color = I('gray50'),
                             showlegend = FALSE)

   # format hovertext for mean
   dm$txt <- with(dm, paste0('Mean:', fmt(Mean), '<br>n=', n,
                             '<br>', miss, ' missing'))

   a <- 0.05
   b <- 0.4
   k <- (a + b) / 6
   w <- (b - a) / 8

   # add mean point
   p <- plotly::add_markers(p,
                            data=dm,
                            mode='markers',
                            color=I('black'),
                            x = ~ Mean,
                            y = ~ y - k,
                            text = ~ txt,
                            hoverinfo = 'text',
                            size=I(10),
                            name='Mean',
                            showlegend=FALSE)

   # function to add segments for the quartiles
   segs <- function(p, x, y, yend, text, data, color, name, width=2) {

      plotly::add_segments(p,
                           data=data,
                           x=x,    y=y,
                           xend=x, yend=yend,
                           text=text,
                           hoverinfo='text',
                           name=name,
                           showlegend=FALSE,
                           color=color,
                           line=list(width=width))
   }

   p <- segs(p, x=~Median,    y=~y-k-w,    yend=~y-k+w,    text=~txt, data=dq1, color=I('gray50'),name='Median', width=3)
   p <- segs(p, x=~quartiles, y=~y-k-w*.8, yend=~y-k+w*.8, text=~txt, data=dq2, color=I('blue'),  name='Quartiles')
   onam <- '0.05, 0.95<br>Quantiles'
   p <- segs(p, x=~outer,    y=~y-k-w*.64, yend=~y-k+w*.64, text=~txt, data=dq3, color=I('red'),  name=onam)

   # connect the quartiles w/ a line
   ys <- -1 - k
   qs <- function(p, x, xend, color, lg){

      plotly::add_segments(p,
                           x=x,   xend=xend,
                           y=~ys, yend=~ys,
                           hoverinfo='none',
                           showlegend=FALSE,
                           alpha=0.3, color=color,
                           name='ignored')
   }

   p <- qs(p, x= ~ qu[1], xend=~ qu[2], color=I('red'),  lg=onam)
   p <- qs(p, x= ~ qu[2], xend=~ qu[4], color=I('blue'), lg='Quartiles')
   p <- qs(p, x= ~ qu[4], xend=~ qu[5], color=I('red'),  lg=onam)

   plotly::layout(p,
                  margin = list(l = 10, r = 10, b = 0, t = 0, pad = 0),
                  xaxis = list(title='',
                               zeroline=FALSE,
                               visible = FALSE,
                               fixedrange = FALSE),
                  yaxis = list(title='',
                               visible = FALSE,
                               tickvals= -1,
                               ticktext = 1,
                               fixedrange = TRUE),
                  hoverlabel = list(font=list(size=12),
                                    align = "left"))%>%
      config(displayModeBar = F) %>%
      partial_bundle()

}


#' @export
create_hist.Date <- create_hist.numeric

#' @export
create_hist.POSIXct  <- create_hist.numeric

#' @export
create_hist.POSIXlt  <- create_hist.numeric

