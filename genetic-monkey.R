library(stringr)

target <- "cognitive science"

pop.size <- 200
mutation.rate <- 0.01
n.generations <- 40

(1/27)^nchar(target)

possible.keys <- c(letters, " ")

create.population <- function(pop.size, target) {
  population <- matrix(sample(possible.keys, pop.size * nchar(target), replace=T), nrow=pop.size)
  return(population)
}

create.population(pop.size, target)

fitness <- function(population, target) {
  fit.val <- rep(0, nrow(population))
  for(r in 1:length(fit.val)) {
    fit.val[r] <- sum(population[r,] == str_split(target, "", simplify=T)) / nchar(target)
  }
  return(fit.val)
}

random.mutation <- function(parent, mutation.rate) {
  for (i in 1: length(parent)) {
    if(rbinom(1,1,mutation.rate)==1) {
      parent[i] <- sample(possible.keys, 1)
    }
  }
  return(parent)
}

next.generation <- function(population, mutation.rate, target) {
  next.pop <- population
  pop.fitness <- fitness(population, target)
  for(i in 1:nrow(next.pop)) {
    parent <- sample(1:length(pop.fitness), 1, prob=pop.fitness)
    child <- random.mutation(parent, mutation.rate)
    next.pop[i,] <- child
  }
}
