
rm(list = ls())

correlator <- function(r, x.mean=0, x.sd=1, y.mean=0, y.sd=1, n=1000){
    x <- rnorm(n, mean=x.mean, sd=x.sd)
    y <- rnorm(n, mean=r*(y.sd/x.sd)*(x-x.mean)+y.mean, sd=sqrt((1-r^2))*y.sd)
    data.frame(x,y)
}

# test matrix_coordinates

mat <- matrix(c(1,0,0,1,0,0,1,0,0), nrow = 3)
matrix_coordinates(mat)


# test id_direct_connections and id_groups

test_that("id_direct_connections only connects stones to at most 4 others in random games", {
  for(i in 1:100){
    x <- sample(1:19, 150, replace = TRUE)
    y <- sample(1:19, 150, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    expect_false( any(colSums(dat) < 1) )
    expect_false( any(colSums(dat) > 5) )
  }
})

test_that("id_groups doesn't have any weird inconsistencies on random games", {
  for(i in 1:40){
    x <- sample(1:19, 100, replace = TRUE)
    y <- sample(1:19, 100, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    moves$group_id <- id_maker(n=nrow(moves), nchar=3)
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    moves$group_id <- id_groups(moves)
    singletons <- moves$group_id[which(colSums(dat) == 1)]
    groupers <- moves$group_id[which(colSums(dat) > 1)]
    multistone_ids <- sort(unique(moves$group_id[duplicated(moves$group_id)]))
    expect_true(!any(singletons %in% multistone_ids))
    expect_true(all(groupers %in% multistone_ids))
  }
})

test_that("id_groups doesn't have any weird inconsistencies on random games", {
  for(i in 1:40){
    x <- sample(1:19, 100, replace = TRUE)
    y <- sample(1:19, 100, replace = TRUE)
    moves <- data.frame(column = x, row = y)
    drop <- which(duplicated(moves))
    if(length(drop)>0) moves <- moves[-drop,]
    moves$group_id <- id_maker(n=nrow(moves), nchar=3)
    dat <- id_direct_connections(moves)
    dat <- dat | t(dat)
    moves$group_id <- id_groups(moves)
    singletons <- moves$group_id[which(colSums(dat) == 1)]
    groupers <- moves$group_id[which(colSums(dat) > 1)]
    multistone_ids <- sort(unique(moves$group_id[duplicated(moves$group_id)]))
    expect_true(!any(singletons %in% multistone_ids))
    expect_true(all(groupers %in% multistone_ids))
  }
})


# test kernelgon

# properly fails if not an n x 2 matrix
# describes a single blob no prob
# various probs all work fine
# describes a correlated blob
# describes an unusually shaped blob
# describes a bimodal distribution

n_obs <- 400
d <- correlator(0.2, x.mean = 2.4, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 400)

plot(d$x, d$y)
kernelgon(d, prob = 0.1, lty = 2)
kernelgon(d, prob = 0.2, lty = 2)
kernelgon(d, prob = 0.3, lty = 2)
kernelgon(d, prob = 0.4, lty = 2)
kernelgon(d, prob = 0.5, lty = 2)
kernelgon(d, prob = 0.6, lty = 2)
kernelgon(d, prob = 0.7, lty = 2)
kernelgon(d, prob = 0.8, lty = 2)
kernelgon(d, prob = 0.9, lty = 2)
kernelgon(d, prob = 0.99, lty = 2)



n_obs <- 400
d <- correlator(-0.8, x.mean = 2.4, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 400)

plot(d$x, d$y)
kernelgon(d, prob = 0.1, lty = 2)
kernelgon(d, prob = 0.2, lty = 2)
kernelgon(d, prob = 0.3, lty = 2)
kernelgon(d, prob = 0.4, lty = 2)
kernelgon(d, prob = 0.5, lty = 2)
kernelgon(d, prob = 0.6, lty = 2)
kernelgon(d, prob = 0.7, lty = 2)
kernelgon(d, prob = 0.8, lty = 2)
kernelgon(d, prob = 0.9, lty = 2)
kernelgon(d, prob = 0.99, lty = 2)




# dealing with two modes!!
# two balls 

n_obs <- 400

base <- correlator(0.1, x.mean = 2.4, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 400)
add <- correlator(-0.1, x.mean = 5.2, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 200)
d <- rbind(base, add)

plot(d$x, d$y)
kernelgon(d, prob = 0.1, lty = 2)
kernelgon(d, prob = 0.2, lty = 2)
kernelgon(d, prob = 0.3, lty = 2)
kernelgon(d, prob = 0.4, lty = 2)
kernelgon(d, prob = 0.5, lty = 2)
kernelgon(d, prob = 0.6, lty = 2)
kernelgon(d, prob = 0.7, lty = 2)
kernelgon(d, prob = 0.8, lty = 2)
kernelgon(d, prob = 0.9, lty = 2)
kernelgon(d, prob = 0.99, lty = 2)




# dealing with a strange shape

n_obs <- 400

base <- correlator(0.6, x.mean = 2.4, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 400)
add <- correlator(-0.6, x.mean = 3.2, x.sd = 0.25, y.mean = -44.9, y.sd = 0.3, n = 200)
d <- rbind(base, add)

# kernelgon problem: 
# all polygons want to be plotted as a single thing..how to seperate??

plot(d$x, d$y)
kernelgon(d, prob = 0.1, lty = 2)
kernelgon(d, prob = 0.2, lty = 2)
kernelgon(d, prob = 0.3, lty = 2)
kernelgon(d, prob = 0.4, lty = 2)
kernelgon(d, prob = 0.5, lty = 2)
kernelgon(d, prob = 0.6, lty = 2)
kernelgon(d, prob = 0.7, lty = 2)
kernelgon(d, prob = 0.8, lty = 2)
kernelgon(d, prob = 0.9, lty = 2)
kernelgon(d, prob = 0.99, lty = 2)

# different resolutions are possible

plot(d)
kernelgon(d, prob = 0.89, border = "black", res = 5)
kernelgon(d, prob = 0.89, border = "blue", res = 0.2)
kernelgon(d, prob = 0.89, border = "red", res = 0.1)


# 



col_alpha <- function (acol, alpha = 0.2){
    acol <- col2rgb(acol)
    acol.red <- acol["red",]/255
    acol.green <- acol["green",]/255
    acol.blue <- acol["blue",]/255
    acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    return(as.character(acol))
}

rose <- c(
  "#FFA8A0", 
  #"#FF7489", 
  "#FF5072", 
  "#FF254D", 
  "#F10033",
  "white"
)


plot(d, col = NULL)
kernelgon(d, prob = 0.89, col = col_alpha(rose[1], 0.5), lty = 2)
kernelgon(d, prob = 0.7, col = col_alpha(rose[2], 0.5), border = NA)
kernelgon(d, prob = 0.5, col = col_alpha(rose[3], 0.5), border = NA)
kernelgon(d, prob = 0.25, col = col_alpha(rose[4], 0.5), border = NA)
points(d)