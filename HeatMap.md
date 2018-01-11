# Heat Map

```

# This code will produce a heat map of win percentage by various troop combinations

k <- 100

matrix.results <- data.frame(matrix(NA, nrow = 20, ncol = 20))
# remaining.red <- data.frame(matrix(NA, nrow = k, ncol = 0))
# remaining.blue <- data.frame(matrix(NA, nrow = k, ncol = 0))

colnames(matrix.results) <- paste0('off', c(1:20))
rownames(matrix.results) <- paste0('def', c(1:20))

for (r in 2:20) {
  for (b in 1:20) {
    
    r.wins <- 0
    b.wins <- 0
    # r.remaining <- c()
    # b.remaining <- c()
    
    repeat{
      
      r.qty <- r
      b.qty <- b
      
      repeat{
        r.roll <- head(sort(sample.int(6, min(r.qty - 1, 3)), decreasing = TRUE), min(r.qty - 1, b.qty, 2))
        b.roll <- head(sort(sample.int(6, min(b.qty, 2)), decreasing = TRUE), min(r.qty - 1, 2))
        r.qty <- r.qty - sum(r.roll == b.roll | r.roll < b.roll, na.rm = TRUE)
        b.qty <- b.qty - sum(r.roll > b.roll, na.rm = TRUE)
        
        if (b.qty == 0){
          r.wins <- r.wins + 1
          # r.remaining <- c(r.remaining, r.qty)
          # b.remaining <- c(b.remaining, b.qty)
          break
        } 
        if (r.qty < 2){
          b.wins <- b.wins + 1
          # r.remaining <- c(r.remaining, r.qty)
          # b.remaining <- c(b.remaining, b.qty)
          break
        }
      }
      
      if (r.wins + b.wins == k){
        r.pct <- 100*(r.wins / k)
        # remaining.red <- cbind(remaining.red, r.remaining)
        # names(remaining.red)[names(remaining.red) == 'r.remaining'] <- paste0('O',r,'D',b)
        # remaining.blue <- cbind(remaining.blue, b.remaining)
        # names(remaining.blue)[names(remaining.blue) == 'b.remaining'] <- paste0('O',r,'D',b)
        break
      }
      
    }
    
    matrix.results[b, r] = r.pct 
    
  }
}
rm(r, b, k)
rm(list = ls(pattern = "^r"))
rm(list = ls(pattern = "^b"))

matrix.results$red1 <- NULL

matrix.results2 <- data.matrix(matrix.results)

heatmap(matrix.results2, 
        scale = "none", 
        Rowv=NA, 
        Colv=NA, 
        col = rev(heat.colors(15)),  
        margins=c(5,10))
        
```
