rows <- 50
cols <- 50
proportion.group.1 <- .5
empty <- .1
min.similarity <- 3/8
pop.size.group.1 <- (rows*cols)*(1-empty)*proportion.group.1
pop.size.group.2 <- (rows*cols)*(1-empty)*(1-proportion.group.1)

initial.population <- sample(c(
  rep(1, pop.size.group.1), 
  rep(2, pop.size.group.2), 
  rep(0, (rows*cols)-pop.size.group.1-pop.size.group.2)
))
grid <- matrix(initial.population, nrow=rows, ncol=cols)

visualize.grid <- function(grid){
  image(grid, col=c('black','red','blue'), xaxs=NULL, yaxs=NULL, xaxt='n', yaxt='n')
}
visualize.grid(grid)

empty.locations <- function(grid){
  return(which(grid==0, arr.ind=T))
}

z <- empty.locations(grid)

similarity.to.center <- function(grid.subset, center.val){
  if(center.val == 0){ return(NA) }
  same <- sum(grid.subset==center.val) - 1
  not.same <- sum(grid.subset!=center.val) - sum(grid.subset==0)
  return(same/(same+not.same))
}

unhappy.agents <- function(grid, min.similarity){
  grid.copy <- grid
  for(row in 1:rows){
    for(col in 1:cols){
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      if(is.na(similarity.score)){
        grid.copy[row,col] <- NA
      } else {
        grid.copy[row,col] <- similarity.score >= min.similarity
      }
    }
  }
  return(which(grid.copy==FALSE, arr.ind = T))
}

one.round <- function(grid, min.similarity){
  empty <- empty.locations(grid)
  unhappy <- unhappy.agents(grid, min.similarity)
  if(length(unhappy)==0){ 
    return(grid) 
  } else {
    new.grid <- grid
    empty <- empty[sample(1:nrow(empty)),]
    for(i in 1:nrow(unhappy)){
      if(i > nrow(empty)){ break; }
      new.grid[empty[i,1],empty[i,2]] <- grid[unhappy[i,1],unhappy[i,2]]
      new.grid[unhappy[i,1],unhappy[i,2]] <- 0
    }
    return(new.grid)
  }
}

done <- FALSE
while(!done){
  new.grid <- one.round(grid, min.similarity)
  if(all(new.grid == grid)){
    done <- TRUE
  } else {
    grid <- new.grid
  }
}
visualize.grid(grid)
