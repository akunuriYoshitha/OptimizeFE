#' Transforms the dataset using the best possible transformation technique
#'
#' Takes in a data frame and performs the best possible transformation to each of the columns in the data frame
#' @param data Any data frame that has atlest one column with continuous data and that has to transformed into normal form
#' @param dv Dependent variable in the dataset.
#' @description
#' Considers the best transformed dataset based on three metrics
#' \itemize{
#' \item Shapiro P Value
#' \item Pearson P Value
#' \item Min skewness
#' }
#' Best possible transformation is applied to all the continous columns in the dataset for each of the above mentioned metrics and three datasets are collected respectively. Best dataset is chosen based on the performance metrics of the dataset on a base model. Original dataset is also considered for the performance measure to actually check if transforming the data is necessary.
#' @return Returns a list of 2 objects:
#' \describe{
#' \item{trans_data}{Tranformed Dataset for all Continuous variables}
#' \item{trans_fit_model}{Model fit file to fit the test data}
#' }
#' @export
#' @usage BestTransform(data, dv)
FEWrapper <- function(data, dv, cols_to_ignore = NULL, date_cols = NULL, freeze_cols = NULL, cat_metric = NULL)
{
  dist <- data_distribution(data, dv)
  cont_cols <- dist[(dist$distribution == "Continous" & dist$is_dv == FALSE),]$names
  dvcol <- data[,dv]
  ppv <- data.table(data)[,lapply(.SD,function(x){VariableTransform(x, "Pearson P Value")$transformed_data}),.SDcols=cont_cols]
  ppv1 <- cbind(ppv, dvcol)

  spv <- data.table(data)[,lapply(.SD,function(x){VariableTransform(x, "Shapiro P Value")$transformed_data}),.SDcols=cont_cols]
  spv1 <- cbind(spv, dvcol)

  skew_v <- data.table(data)[,lapply(.SD,function(x){VariableTransform(x, "Min skewness")$transformed_data}),.SDcols=cont_cols]
  skew_v1 <- cbind(skew_v, dvcol)

  # original <- data[,cont_cols]
  # original1 <- cbind(original, dvcol)

  dv_index <- which(colnames(data) == dv)
  colnames(data)[dv_index] <- "dvcol"
  original1 <- data

  total_matrix_all <- list("Pearson P Value" = ppv1, "Shapiro P Value" = spv1, "Min skewness" = skew_v1, "Original" = original1)
  # library(pbmcapply)
  output<-do.call(rbind,pbmclapply(seq(1:4), model_my_data,
                                   data = total_matrix_all,
                                   mc.cores = 1))
  if (dist[dist$is_dv == T, ]$distribution == "Continous")
  {
    output <- data.frame(output[, c("Rsquared", "MAE", "RMSE")])
    var <- c("Rsquared", "MAE", "RMSE")
  } else
  {
    output <- data.frame(output[, c("Mean_F1", "Mean_Precision", "Mean_Recall")])
    var <- c("Mean_F1", "Mean_Precision", "Mean_Recall")
  }

  # var<-nearZeroVar(output, saveMetrics = TRUE)
  # var<-rownames(var[(var$zeroVar=="FALSE"),])
  # out<-data.frame(output)[,var]
  output$Method <- names(total_matrix_all)
  best_trans_metric <- names(total_matrix_all[which(output[,1] == max(output[,1]))[1]])
  # best_trans_metric <- "Pearson P Value"  ## Delete this line after testing
  if (best_trans_metric == "Original")
  {
    trans_data <- total_matrix_all[[best_trans_metric]]
    trans_fit_model <- NULL
  } else
  {
    trans_data <- total_matrix_all[[best_trans_metric]]
    possible_fits <- list()
    chosen_transforms <- list()
    for (i in 1:length(cont_cols))
    {
      # print(i)
      res <- VariableTransform(data[,cont_cols[i]], best_trans_metric)
      chosen_transforms[[cont_cols[i]]] <- res$best_transformation
      possible_fits[[cont_cols[i]]] <- res$possible_fits
    }

    trans_fit_model <- list(chosen_transforms = chosen_transforms, possible_fits = possible_fits)
  }
  output <- output[,c("Method", var)]
  return(list(trans_data = trans_data, trans_fit_model = trans_fit_model, model_perf_metrics = output))

}
