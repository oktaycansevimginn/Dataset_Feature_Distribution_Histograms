

#OKTAY CAN SEVIMGIN

main_data <- read.delim("C:\\Users\\Oktay Can\\Desktop\\Dataset\\DatasetNA.txt", sep=" ", header=TRUE)

columns_to_convert <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")

for (column in columns_to_convert) {
  main_data[[column]] <- as.numeric(main_data[[column]])
}

histogram_custom3 <- function(data, columns, bins = 10, 
                              main_title = "Histograms", 
                              xlab = "Values", 
                              ylab = "Frequency", 
                              col = "red") {
  
  num_vars <- length(columns)  
  
  rect_manual <- function(xleft, ybottom, xright, ytop, col = "red", border = "black") {
    segments(xleft, ybottom, xright, ybottom, col = border)
    segments(xright, ybottom, xright, ytop, col = border)
    segments(xright, ytop, xleft, ytop, col = border)
    segments(xleft, ytop, xleft, ybottom, col = border)
    
    fill_step <- (xright - xleft) / 50
    for (x in seq(xleft, xright, by = fill_step)) {
      segments(x, ybottom, x, ytop, col = col)
    }
  }
  
  num_rows <- 3
  num_cols <- 3
  par(mfrow = c(num_rows, num_cols)) 
  
  for (i in seq_along(columns)) {
    column <- columns[i]
    values <- na.omit(data[[column]])
    
    if (length(values) == 0 || length(unique(values)) == 1) next
    
    min_val <- min(values)
    max_val <- max(values)
    bin_width <- (max_val - min_val) / bins
    breaks <- seq(min_val, max_val, by = bin_width)
    counts <- numeric(length(breaks) - 1)
    
    for (j in seq_along(counts)) {
      counts[j] <- sum(values >= breaks[j] & values < breaks[j + 1])
    }
    
    counts[length(counts)] <- counts[length(counts)] + sum(values == max_val)
    
    xlim <- c(min_val, max_val)
    ylim <- c(0, max(counts) * 1.1)
    
    plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = column, xaxt = "n", yaxt = "n")
    for (j in seq_along(counts)) {
      rect_manual(breaks[j], 0, breaks[j + 1], counts[j], col = col, border = "black")
    }
    axis(1, at = breaks, labels = round(breaks, 2))
    axis(2)
  }
  
  plot.new()
  title(main = main_title, outer = TRUE, line = -1)
  
  par(mfrow = c(1, 1))  
  
  for (column in columns) {
    par(mfrow = c(1, 1)) 
    
    values <- na.omit(data[[column]])  
    if (length(values) == 0 || length(unique(values)) == 1) next
    
    min_val <- min(values)
    max_val <- max(values)
    bin_width <- (max_val - min_val) / bins
    breaks <- seq(min_val, max_val, by = bin_width)
    counts <- numeric(length(breaks) - 1)
    
    for (j in seq_along(counts)) {
      counts[j] <- sum(values >= breaks[j] & values < breaks[j + 1])
    }
    
    counts[length(counts)] <- counts[length(counts)] + sum(values == max_val)
    
    xlim <- c(min_val, max_val)
    ylim <- c(0, max(counts) * 1.1)
    
    plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = paste(column, "Histogram"), xaxt = "n", yaxt = "n")
    for (j in seq_along(counts)) {
      rect_manual(breaks[j], 0, breaks[j + 1], counts[j], col = col, border = "black")
    }
    axis(1, at = breaks, labels = round(breaks, 2))
    axis(2)
    
    Sys.sleep(1)
  }
}

columns_to_plot <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
histogram_custom3(main_data, columns = columns_to_plot, bins = 10, main_title = "Veri Seti HistogramlarD1")
