# clear memory
rm(list = ls())

library(doParallel)
library(parallel)
library(Rcpp)
library(dplyr)


sysinf <- Sys.info()
if (tolower(sysinf[['sysname']]) == "windows") {
    # windows of mac osx
    n_cores = 1
} else {
    # linux
    n_cores = 4
    library(gputools)
}

# default values
num_clusters = 3

mat_prod_lim = 100
num_iterations = 20
k_means_iter = 100
euc_f = 'euc_dist_r'
cp_f = 'tcrossprod'
printgranular = FALSE
city = "sf"


load("ch_data.RData")
load("sf_data.RData")
t_data = NULL
if (city == "ch") {
    t_data = ch_data
} else {
    t_data = sf_data
}
index_col = 2
t_data = arrange(t_data, t_data$idx)

################################################
# Euclidean Distance using Matrix Multiplication
################################################
euc_dist_crossprod <- function(dat, centers, tcrossprod_f, mat_prod_lim=50) {
    # no. of rows in centers should match number of rows in dat
    start.time <- Sys.time()
    parallel.euc_dis <- function(i) {
        startidx = (i*mat_prod_lim)+1
        endidx = min(dim(dat)[1], (i+1)*mat_prod_lim)
        subdat = dat[startidx:endidx, ] - centers[startidx:endidx, ]
        return(diag(tcrossprod_f(subdat, subdat))^(1/2))
    }
    result = unlist(mclapply(0:(dim(dat)[1]%/%mat_prod_lim), FUN=parallel.euc_dis, 
                      mc.cores=n_cores))
    time.taken <- Sys.time() - start.time
    if (printgranular) cat("euc_dist_crossprod runtime: ", time.taken, "\n")
    return(result)
}

################################################
# Euclidean Distance C++ Implementation
################################################
sourceCpp('./euc_dist_c.cpp')

############
# dat and centers are 2 dimensional matrixes 
############
euc_dist_r <- function(dat, centers) {
    #start.time <- Sys.time()
    res = rowSums((dat - centers)^2)^(1/2)
    #time.taken <- Sys.time() - start.time
    if (printgranular) cat("euc_dist_R runtime: ", time.taken, "\n")
    return(res)
}


# read in the arguments listed at the command line
args=(commandArgs(TRUE))
if(length(args) != 0){
    for(i in 1:length(args)){
        eval(parse(text=args[[i]]))
    }
}

# print script settings
cat("num_iterations=", num_iterations, "  n_cores=", n_cores, 
    "  k_means_iter=", k_means_iter, "  euc_f=", euc_f, "  cp_f=", cp_f, 
    "  mat_prod_lim=", mat_prod_lim, "\n")



kmeans_custom <- function(dat, init_centers, iter_max=10, dist_f, ...) {
    centers = init_centers
    n_centers = dim(centers)[1]
    
    dists_cent_i <- function(i) {
        res = dist_f(dat, matrix(rep(centers[i, ], dim(dat)[1]), 
                           nrow=dim(dat)[1], byrow=TRUE), ...)
        return(res)
    }
    dists = sapply(mclapply(1:(dim(centers)[1]),
                            FUN=dists_cent_i, mc.cores=n_cores),
                   function(x) {return(x)})
    row.names(dists) = rownames(dat)
    idx_center = apply(dists, 1, which.min)
    iter = 0
    repeat{
        # update cluster centers
        center_f = factor(idx_center, levels=1:n_centers)
        centervals = split(idx_center, center_f)
        centers = foreach(i=1:(dim(centers)[1]), .combine = rbind) %do% {
            if (length(centervals[[i]]) == 1) {
                dat[names(centervals[[i]]), ]
            } else {
                colMeans(dat[names(centervals[[i]]), ])
            }
        }
        new_dists = sapply(mclapply(1:(dim(centers)[1]),
                                    FUN=dists_cent_i, mc.cores=n_cores),
                           function(x) {return(x)})
        row.names(new_dists) = rownames(dat)
        
        new_idx_center = apply(new_dists, 1, which.min)
        if(isTRUE(all.equal(new_idx_center, idx_center))) {
            break 
        }
        iter = iter + 1
        if(iter > iter_max) {
            warning("K-Means did not converge")
            break
        }
        dists = new_dists
        idx_center = new_idx_center
    }
    new_idx_center
}


cat("Monte-Carlo K-Means\n")
start.time <- Sys.time()
    parallel.kmeans <- function(i) {
        dat = as.matrix(t_data)
        rownames(dat) = dat[, index_col]
        dat = dat[sample(nrow(dat)), ]
        # random sample to init centers
        init_centers = dat[sample(nrow(dat), num_clusters), ]

        if(euc_f == 'euc_dist_r') {
            result = kmeans_custom(as.matrix(dat), init_centers, 
                                   iter_max=k_means_iter, euc_dist_r)
        } else if (euc_f == 'euc_dist_c') {
            result = kmeans_custom(as.matrix(dat), init_centers, 
                                   iter_max=k_means_iter, euc_dist_cpp)
        } else {
            result = kmeans_custom(as.matrix(dat), init_centers, 
                                   iter_max=k_means_iter, euc_dist_crossprod, 
                                   get(cp_f), mat_prod_lim)
        }
        return(data.frame(idx=dat[, index_col], cluster=as.vector(result)))
    }
    results = mclapply(rep(num_clusters, num_iterations), FUN=parallel.kmeans,
                       mc.cores=n_cores)
time.taken <- Sys.time() - start.time
cat("K-Means MC runtime: ", time.taken, "\n")


cat("Aggregate M.C. Results\n")
results = lapply(results, function(result) {result[order(result$idx), ]})
agg_clus = cbind(results[[1]][, 1], sapply(results, function(x) {x[, 2]}))
summer_obs = agg_clus[, 1] > 8 & agg_clus[, 1] < 8.5
winter_obs = agg_clus[, 1] > 12
calc_mode <- function(dat_row) {
    vals <- unique(dat_row)
    vals[which.max(tabulate(match(dat_row, vals)))]
}
# fix k-means cluster order randomness
parallel.fixLabels <- function(i){
    summer_lab = calc_mode(agg_clus[summer_obs, i])
    winter_lab = calc_mode(agg_clus[winter_obs, i])
    result = agg_clus[, i]
    result[result == summer_lab] = -2
    result[result == winter_lab] = -1
    result[result != -1 & result != -2] = -3
    result = result * -1
}
fixed_results = sapply(mclapply(2:dim(agg_clus)[2], FUN=parallel.fixLabels,
                                mc.cores=n_cores), cbind)

consecutive_matches <- function(arr, n=7){
    res = arr
    res[1:n-1] = 0
    for(i in n:length(arr)){
        res[i] = all(arr[(i-n+1):i] == 2)
    }
    res
}

cols = consecutive_matches(apply(fixed_results, 1, calc_mode))
cols[cols == 1] = 2 # red
cols[cols == 0] = 1 # black

load("sf_data_date.RData")

length(t_data$idx)
dim(sf_data)

plot(x=sf_data$date, y=t_data$temp, col=cols, main = "San Francisco Temperatures (2014-2017)", xaxt = "n", xlab = "Time of Year", ylab = "Temperature (F)")
axis(1, at=seq(min(sf_data$date), max(sf_data$date), by="months"), 
          labels=FALSE)
text(seq(1, 44, by=1), par("usr")[3] - 0.2, labels = x, srt = 45, pos = 1, xpd = TRUE)



lablist<-as.vector(c(1:10))
axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)


axis(1, xaxp=c("Jan", "Feb"), las=2)

idx_1 = min(which(cols == 2))
idx_2 = max(which(cols == 2))

variance = 0
non_empty_clusters = unique(apply(fixed_results,1,calc_mode))
for (i in non_empty_clusters) {
    clus_obs = apply(fixed_results,1,calc_mode)==i
    obs_idxs = agg_clus[clus_obs, 1]
    obs_temps = t_data[clus_obs, ]$temp
    center_x = mean(obs_idxs)
    center_y = mean(obs_temps)
    variance = variance + sum((obs_idxs - center_x)^2) + sum((obs_temps - center_y)^2)
}

cat("Start of Summer:", agg_clus[idx_1, 1], "\n")
cat("End of Summer:", agg_clus[idx_2, 1], "\n")
cat("Std. Dev.:", sqrt(variance), "\n")

