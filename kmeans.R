# clear memory
rm(list = ls())

library(parallel)

# default values
sysinf <- Sys.info()
if (tolower(sysinf[['sysname']]) == "windows") {
    # windows of mac osx
    n_cores = 1
} else {
    # linux
    n_cores = 4
}
num_iterations = 40
k_means_iter = 100
do_shuffle = TRUE


# read in the arguments listed at the command line
args=(commandArgs(TRUE))
if(length(args) != 0){
    for(i in 1:length(args)){
        eval(parse(text=args[[i]]))
    }
}
# print script settings
cat("num_iterations=", num_iterations, "  n_cores=", n_cores, 
    "  k_means_iter=", k_means_iter, "\n")


cat("Reading Data into Memory\n")
system.time({
    temp_data <- read.csv('data/Beach_Weather_Stations_-_Automated_Sensors.csv')
})
    

cat("Data Cleaning\n")
system.time({
    temp_data$date = as.Date(temp_data[,2], "%m/%d/%Y")
    temp_data$idx = as.numeric(format(temp_data$date, "%m")) + 
        as.numeric(format(temp_data$date, "%d")) / 31
    temp_data = temp_data[!is.na(temp_data[,4]), ]
})


cat("Monte-Carlo K-Means\n")
num_clusters = 3
system.time({
    # k means is sensitive to data order
    # add shuffle make monte carlo meaningful
    shuffle <- function(data_df) {
            if(do_shuffle) {
                return(data_df[sample(nrow(data_df)), ])
            } else {
                return(data_df)
            }
    }
    parallel.kmeans <- function(i) {
        shuffled_data = shuffle(temp_data[, c('idx', "Wet.Bulb.Temperature")])
        result = kmeans(x=shuffled_data, iter.max=k_means_iter, centers=num_clusters,
                        algorithm='Lloyd')
        return(data.frame(idx=shuffled_data['idx'], cluster=as.vector(result$cluster)))
    }
    results = mclapply(rep(num_clusters, num_iterations), FUN=parallel.kmeans,
                        mc.cores=n_cores)
})

cat("Aggregate M.C. Results\n")
system.time({
    results = lapply(results, function(result) {result[order(result$idx), ]})
    agg_clus = cbind(results[[1]][, 1], sapply(results, function(x) {x[, 2]}))
    summer_obs = agg_clus[, 1] > 6 & agg_clus[, 1] < 8
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
    idx_1 = min(which(apply(fixed_results, 1, function(x) all(x == 2))))
    idx_2 = max(which(apply(fixed_results, 1, function(x) all(x == 2))))
})

cat("Start of Summer:", agg_clus[idx_1, 1], "\n")
cat("End of Summer:", agg_clus[idx_2, 1], "\n")
cat("Variance:", mean(apply(fixed_results,1,var)), "\n")


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
plot(x=t_data$idx, y=t_data$temp, col=cols)

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
