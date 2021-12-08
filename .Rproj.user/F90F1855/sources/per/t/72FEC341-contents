pretty.output <- function(summary, independent.var.value, analysisplan) {
  subset <- summary[which(summary$independent.var.value == independent.var.value),]
  independent.var <- subset$independent.var[1]
  if(is.na(independent.var)) {
    analplan_subset <- analysisplan
  } else {
    analplan_subset <- analysisplan[which(analysisplan$independent.variable == independent.var),]
  }
  vars <- unique(subset$dependent.var)
  # districts <- unique(subset$repeat.var.value)
  # start <- ifelse(camp, 1, 19)
   df <- data.frame(governorate = "gaza",  
                    district = "gaza", stringsAsFactors = F)
 
   for(i in 1:length(vars)){
     var_result <- subset[which(subset$dependent.var == vars[i]),]
     df[,vars[i]] <- var_result$numbers
     df[,sprintf("%s_min", vars[i])] <- var_result$min
     df[,sprintf("%s_max", vars[i])] <- var_result$max
   } 
  extra_heading <- data.frame(t(vars), stringsAsFactors = F)
  colnames(extra_heading) <- vars
  extra_heading[1,] <- t(analplan_subset[,1][match(vars, analplan_subset$dependent.variable)])
  extra_heading[2,] <- t(analplan_subset$research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[3,] <- t(analplan_subset$sub.research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[4,] <- t(analplan_subset$dependent.variable.type[match(vars, analplan_subset$dependent.variable)])
  # if (severity){
  #   extra_heading[5,] <- t(analplan_subset$consequence[match(vars, analplan_subset$dependent.variable)])
  # }
  df <- rbind.fill(df, extra_heading)
  df <- df[c((nrow(df)-(nrow(extra_heading) - 1)):nrow(df),1:(nrow(df)-nrow(extra_heading))),]
  # df$district <- lookup_table$english[match(df$district, lookup_table$name)]
  # if(!camp){df$governorate <- lookup_table$english[match(df$governorate, lookup_table$name)]}
  df[1:nrow(extra_heading), which(is.na(df[1,]))] <- ""
  df
}

correct.zeroes <- function(summary) {
  zeroes <- which(summary$dependent.var.value == 0 & summary$numbers == 1)
  summary$dependent.var.value[zeroes] <- 1
  summary$numbers[zeroes] <- 0
  summary$min[zeroes] <- 0
  summary$max[zeroes] <- 0
  return(summary)
}



analysisplan_nationwide <- function(analysisplan) {
  analysisplan$repeat.for.variable <- ""
  return(analysisplan)
}
analysisplan_pop_group_aggregated <- function(analysisplan) {
  analysisplan$independent.variable <- ""
  analysisplan$independent.variable.type <- ""
  return(analysisplan)
}

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
