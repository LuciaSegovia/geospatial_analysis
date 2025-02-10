
library(foreach) # for parallel processing
library(doParallel) # for parallel processing
library(geosphere) # for calculating distances
library(sf) # for reading in and writting shapefiles
library(here) # for working directory management
library(tidyverse) # for data wrangling

# start timer to measure the time it takes to run the script
start_time <- Sys.time()

# set working directory to the root of the project. Everything else is relative to this.
wd <- here()

# Delete temorary files in "data/variogram_outputs/tmp" if they exist
if (length(list.files(path = file.path(wd, "data/variogram_outputs/tmp"), pattern = "mwi_sample_points_variogram_partial", full.names = TRUE)) > 0) {
    file.remove(list.files(path = file.path(wd, "data/variogram_outputs/tmp"), pattern = "mwi_sample_points_variogram_partial", full.names = TRUE))
}


# read in the sample points shapefile.
sample_points <- st_read(file.path(wd, "data/point_outputs/mwi_sample_points.shp"))

# # subset the sample points to the first 100 points for testing
sample_points <- sample_points |>
    st_drop_geometry() |> # drop the geometry column
    as_tibble() # convert to a tibble for easier manipulation
# head(10)

# set pH and log SOC variogram parameters
pH_cn <- 0.198 # nugget variance (uncorrelated)
pH_c1 <- 0.253 # correlated variance
pH_phi <- 36.81 # distance parameter (km)
pH_kappa <- 0.5 # smoothness parameter

log_SOC_cn <- 0.204 # nugget variance (uncorrelated)
log_SOC_c1 <- 0.075 # correlated variance
log_SOC_phi <- 43.34 # distance parameter (km)
log_SOC_kappa <- 0.5 # smoothness parameter

# define the Matérn variogram function
matern <- function(u, phi, kappa) {
    # set names to NULL to avoid issues with empty vectors
    if (is.vector(u)) {
        names(u) <- NULL
    }
    # set dimnames to NULL to avoid issues with empty matrices
    if (is.matrix(u)) {
        dimnames(u) <- list(NULL, NULL)
    }
    # calculate the Matérn correlation
    uphi <- u / phi
    uphi <- ifelse(u > 0, (((2^(-(kappa - 1))) / ifelse(0, Inf,
        gamma(kappa)
    )) * (uphi^kappa) * besselK(x = uphi, nu = kappa)),
    1
    )
    uphi[u > 600 * phi] <- 0
    return(uphi)
}

# define the Matérn variogram function
vgm.mat <- function(h, phi, kappa, cn, c1) {
    # calculate the variogram for lag h
    rho <- matern(h, phi, kappa)
    return(cn + c1 * (1 - rho)^2)
}

# initialize a dataframe to store the results of the pairwise computations. I keep the EA code and district name for each location to make it easier to identify the locations in the results.
results <- tibble(
    p1_id = character(),
    p1_EACODE = character(),
    p1_District = character(),
    p2_id = character(),
    p2_EACODE = character(),
    p2_District = character(),
    h = numeric(),
    ph_variogram = numeric(),
    log_SOC_variogram = numeric()
)

# set up a cluster with n cores for parallel processing. I use n - 1 cores to leave one core free for other processes.
cl <- makeCluster(7)

# register the cluster for parallel processing
registerDoParallel(cl)



# loop through the sample points and calculate the pairwise variograms in parallel
results <- foreach(i = 1:nrow(sample_points), .combine = rbind) %dopar% {
    result_list <- list()
    for (j in 1:nrow(sample_points)) {
        # skip the computation if the two locations are the same
        if (i < j) {
            # calculate the distance between the two locations in km using the Vincenty ellipsoid method
            loc1 <- c(sample_points$xcoord[i], sample_points$ycoord[i])
            loc2 <- c(sample_points$xcoord[j], sample_points$ycoord[j])
            h <- geosphere::distVincentySphere(loc1, loc2) / 1000 # distance converted to km

            # Calculate the PH variogram for the two locations using the Matérn variogram function
            ph_variogram <- vgm.mat(h, pH_phi, pH_kappa, pH_cn, pH_c1)
            # Calculate the log SOC variogram for the two locations using the Matérn variogram function
            log_SOC_variogram <- vgm.mat(h, log_SOC_phi, log_SOC_kappa, log_SOC_cn, log_SOC_c1)

            # store the in a dataframe
            result <- dplyr::tibble(
                p1_id = sample_points$id[i],
                p1_EACODE = sample_points$EACODE[i],
                p1_District = sample_points$DISTRICT[i],
                p2_id = sample_points$id[j],
                p2_EACODE = sample_points$EACODE[j],
                p2_District = sample_points$DISTRICT[j],
                h = h,
                ph_variogram = ph_variogram,
                log_SOC_variogram = log_SOC_variogram
            )
            # append the result to the results dataframe
            result_list[[j]] <- result
            # # write the results to a CSV file after each iteration
            # readr::write_csv(do.call(rbind, result_list), paste0(wd, "/data/variogram_outputs/tmp/mwi_sample_points_variogram_partial_", i, ".csv"))

            # # write the results to a CSV file after each iteration and append to the existing file
            # readr::write_csv(do.call(rbind, result_list), paste0(wd, "/data/variogram_outputs/tmp/mwi_sample_points_variogram_merged.csv"), append = TRUE)
        }
    }
    # print a message to the console to show progress
    message(paste0("Finished ", i, " of ", nrow(sample_points), " locations"))
    return(do.call(rbind, result_list))
}

# stop the cluster and deregister the parallel backend
stopCluster(cl)

# # read all the partial CSV files and combine them into a single dataframe
# partial_results <- list.files(path = file.path(wd, "data/variogram_outputs/tmp"), pattern = "mwi_sample_points_variogram_partial", full.names = TRUE)


# results <- dplyr::bind_rows(lapply(partial_results, read_csv))

# write the results to a csv file. Replace file if it already exists.
write_csv(results, file.path(wd, "data/variogram_outputs/mwi_sample_points_variogram.csv"))

# stop the timer
end_time <- Sys.time()

# Print where the results are stored
cat(paste0("The script took ", round((end_time - start_time), 2), " minutes to run and process ", nrow(sample_points), " points and the results are stored in: \n", file.path(wd, "data/variogram_outputs/mwi_sample_points_variogram.csv")))