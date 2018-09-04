# Time Series Clustering using K-Means Algorithm and Performance Optimizations with Rcpp and GPUTools

## Abstract

In this project I explored the clustering and computing performance of the K-Means algorithm. In the final report, I began by discussing the algorithmic complexity and nuances of K-Means (Lloyd’s Algorithm). This section was concluded by conducting a case study that employs clustering and Monte-Carlo simulation on time series data. 

In the second section of the report, I implemented one computationally complex subtask of the K-Means algorithm in 3 different programming archetypes. The performance of an iterative Rcpp implementation is compared against a vectorized R solution. Next, I introduce some facts about running R programs on umich’s Flux GPU cluster, and summarize the performance of an R implementation that takes advantage of GPU acceleration. I close with a short discussion on the types of problems that are best suited for each programming archetype. 

Please see the [include report](./report.pdf) for details 
