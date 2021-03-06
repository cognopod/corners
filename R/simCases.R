#' simCases
#' 
#' simCases is a function to generate simluated data for mvQCA analysis
#' 
#' @param varTypes A vector (created using the combine or C() function) of variable types. B is a binary variable (0 or 1). M is a multi-value variable (currently 0, 1, or 2).  C is a cluster variable that will create two continuous variables based on four underlying clusters. Right now only one C variable is allowed. Defaults to four variables--three binary + one multivalue.
#' @param complexity controls how complex the solutions are by adjusting the frequency of 'not' solutions. Must be greater than 2. Default is 10.
#' @param ratio controls how complex the solutions are by adjusting the ratio of contributing variables to ignored variables in solutions. As ratio approaches 1, more variables are ignored. As ratio increases, fewer variables are ignored. DO NOT enter values less than or equal to one. Default is 2.
#' @param numSolutions is the *maximum* number of paths/solutions leading to the outcome. simCases will remove redundancies and null solutions before outputting the final set of unique solutions, so it is quite likely that a given set of solutions will not reach the maximum. Must be at least one. Default is 6.
#' @param numCases is the number of cases to include in the simulated data set. Must be greater than 1. Default is 50.
#' @param noiseCases is the number of *additional* randomly generated cases used to examine how unknown variables affect the solution. Default is 0.
#' @param distribute determines whether some proportion of the cases are ensured to be the target configurations. Default is FALSE.
#' @param distProp is the proportion of cases that are ensured to be target configurations if distribute is true. Default is 0.50.
#' @return A list with components
#'    \item{paths}{A data frame with one column (PATHS) that lists the paths retained after removing redundancies and null (nonexistent in the simulated data) paths. Paths are described using curly-bracket notation: factor\{levels\}.}
#'    \item{opaths}{A dataframe with one column (PATHS) that lists the originally generated paths including those that could have existed but may not be present in the final path list have due to nonexistent cases. Paths are described using curly-bracket notation: factor\{levels\}.}
#'    \item{caseData}{A dataframe that includes all of the cases in the simulated dataset.  Levels are indicated by number. A C variable will generate two columns (.e.g D1 and D2) with continuous numbers between 0 and 1. The outcome variable is binary and is indicated by the OUT column.}
#'    \item{clusters}{A vector with the cluster membership for each case if a C variable is requested.}
#'    \item{allSolutions}{A dataframe with the full set of possible paths generated by simCases. Levels are indicated by number; -1 refers to any level.}
#'    \item{uniqueSolutions}{A dataframe with the simplest paths that cover all generated solutions with existing case data.  Levels are indicated by number; -1 refers to any level.}
#'    \item{redundantSolutions}{A dataframe with redundant (more complex) paths that are covered by a simpler unique path. Levels are indicated by number; -1 refers to any level.}
#'    \item{comparator}{A data frame with information currently used for checking the redundancy reduction algorithm. See code for elements.}
#' @export
#' 
simCases <- function(varTypes = c("B", "B", "B", "M"), complexity = 10, ratio = 2, numSolutions = 6, numCases = 50, noiseCases = 0, distribute = FALSE, distProp = 0.50) {
  # calculate the number of variables from the length of the variable type vector
  numVars = length(varTypes)
  # generate a random preSolution data frame--used to vary the complexity of the resulting solutions
  preSolutions <- data.frame(replicate(numVars,sample(0:complexity,numSolutions,rep=TRUE)))
  # name the columns for ease of running QCA
  colnames(preSolutions) <- LETTERS[1:numVars]
  # a function that, depending on variable type, generates possible solutions
  # called via apply() below, passes indices for row and column to iterate over variables
  convertSolution <- function(i.row, i.col) {
    x <- preSolutions[i.row,i.col];
    # for binary (0 or 1) variables
    if (varTypes[i.col] == "B")
      # 'not' outcomes are assigned to 0
      # the others are split at the ratio point and assigned to
      # -1 for ignored variables in that solution or
      # 1 for included variables
      if (x > 0) (if (x < complexity/ratio) -1 else 1) else 0
    # for multi-value variables
    # the split here is into 3 levels instead of 2 (plus the -1 'ignore' flag)
    # probably need a different approach than nested if/else to go higher
    else if (varTypes[i.col] == "M")
      if (x > 0) (if (x < complexity/ratio) -1 else if(x > complexity*((ratio + 1)/(2*ratio))) 2 else 1) else 0
    # for the clustering variables, we also use the -1 to 2 range, although
    # -1 isn't used as an ignore flag here, and the numbers are incremented by 1 in the 
    # outputed case data (so 0-3)
    # probably should ignore in solutions at some point
    else if (varTypes[i.col] == "C")
      # if (x > 0) (if (x < complexity/ratio) -1 else if(x > complexity*((ratio + 1)/(2*ratio))) 2 else 1) else 0
      if (x > 0) (if (x < complexity/ratio) 0 else if(x < complexity/ratio*2) 1 else if(x < complexity/ratio*3) 2 else 3) else -1
  }
  # now that we have the function, let's generate some solutions with nested apply statements (rows, then columns to get indices)
  # we need to go cell by cell, but to check the variable type as well
  genSolutions <- as.data.frame(apply(as.data.frame(c(1:nrow(preSolutions))), 1, FUN = function(x) {apply(as.data.frame(c(1:ncol(preSolutions))), 1, FUN=convertSolution, i.row = x)}))
  # well, that generates a transposed set of solutions, so let's transpose it and turn it into a data frame
  genSolutions <- as.data.frame(t(as.matrix(genSolutions)))
  # we need these as integers to use identical() later, so let's clean that up
  genSolutions <- as.data.frame(apply(genSolutions,1:2,FUN = function(x) {as.integer(x)}))
  # name the columns for easy use in QCA
  colnames(genSolutions) <- LETTERS[1:numVars]
  # reset the row names
  row.names(genSolutions) <- NULL
  # create a redundant variables
  redundant <- NULL
  # create a data frame for examining the results of the redundancy checker
  # don't really need this, but outputting it for testing purposes right now
  comparator <- data.frame(matrix(NA, nrow = nrow(genSolutions)*nrow(genSolutions), ncol = 8))
  # name the columns in comparator
  colnames(comparator) <- c("i", "j", "i.masked", "j.masked", "mask.match", "i.NULL", "j.NULL", "IDENTICAL")
  # check every solution against every other, because we need to test for all subset relations
  for (i in 1:nrow(genSolutions)) {
    for (j in 1:nrow(genSolutions)) {
      # build the comparator output
      # the superset
      comparator[((i-1)*nrow(genSolutions)+j),1] <- i
      # the subset
      comparator[((i-1)*nrow(genSolutions)+j),2] <- j
      # superset masked (all -1s removed)
      comparator[((i-1)*nrow(genSolutions)+j),3] <- paste0(unlist(genSolutions[i,which(genSolutions[i,] >= 0)]),collapse="")
      # subset masked by superset
      comparator[((i-1)*nrow(genSolutions)+j),4] <- paste0(unlist(genSolutions[j,which(genSolutions[i,] >= 0)]),collapse="")
      # are the two masked sets identical? if so, j is a subset of i and is redundant
      comparator[((i-1)*nrow(genSolutions)+j),5] <- identical(unlist(genSolutions[i,which(genSolutions[i,] >= 0)]),unlist(genSolutions[j,which(genSolutions[i,] >= 0)]))
      # check for superset as NULL set
      comparator[((i-1)*nrow(genSolutions)+j),6] <- sum(genSolutions[i,]) > ncol(genSolutions)*-1
      # check for subset as NULL set
      comparator[((i-1)*nrow(genSolutions)+j),7] <- sum(genSolutions[j,]) > ncol(genSolutions)*-1
      # check for completely identical sets (which are also redundant, but will cancel each other out here and thus have to be removed another way)
      comparator[((i-1)*nrow(genSolutions)+j),8] <- identical(unlist(genSolutions[i,]),unlist(genSolutions[j,])) == FALSE
      # now the actual test using the four Boolean tests immediately above
      if (identical(unlist(genSolutions[i,which(genSolutions[i,] >= 0)]),unlist(genSolutions[j,which(genSolutions[i,] >= 0)])) && sum(genSolutions[j,]) > ncol(genSolutions)*-1 && sum(genSolutions[i,]) > ncol(genSolutions)*-1 && identical(unlist(genSolutions[i,]),unlist(genSolutions[j,])) == FALSE) {
        # make a vector of all of the redundant subsets
        redundant[length(redundant) + 1] = j;
      }
    }
  }
  # test if there are any redundant subsets, then sort them for readability
  # sort is probably not necessary, added to fix a problem with something else
  # didn't fix the problem
  # or did it? I don't remember.
  if (length(redundant) > 0 ) {
    redundant <- sort(unique(redundant))
    # use bracket subsetting to generate a list of redundant solutions
    redundantSolutions <- genSolutions[redundant,]
    # and a list of unique solutions
    # which contains NULL sets still
    # and identical sets
    uniqueSolutions <- genSolutions[-redundant,]
    # if there are no redundancies, just output a NULL set of redundant solutions
    # and the unique solutions come from the full set
  } else {
    redundantSolutions <- NULL
    uniqueSolutions <- genSolutions
  }
  # the next section needs to reduce the unique solution down to the prime implicants
  # we'll create a variable (called expandSolutions, although it does nothing of the sort)
  # need to get rid of redundant solutions at this point, though, or there will be bad times
  # also the NULL set (that is what the which() does)
  expandSolutions <- unique(uniqueSolutions[which(rowSums(uniqueSolutions) > ncol(uniqueSolutions)*-1),])
  # but let's save a version of the solutions that are entered into this process for testing
  originalSolutions <- unique(uniqueSolutions[which(rowSums(uniqueSolutions) > ncol(uniqueSolutions)*-1),])
  # a variable to reset to stop the while loop
  cd = 1
  # while we are still minimizing...
  while (cd > 0 ) {
    # a variable used to examine whether we got through the entire sequence without interruptions
    clean = TRUE
    # compar each solulation to each other solution
    for (i in 1:nrow(expandSolutions)) {
      for (j in 1:nrow(expandSolutions)) {
        # create a comparison, masking out the unspecified variables in i (in both i and j)
        # we're looking for single differences (a sum of 1 across the remaining locations)
        diffSols <- abs(unlist(expandSolutions[i,which(expandSolutions[i,] >= 0)]) - unlist(expandSolutions[j,which(expandSolutions[i,] >= 0)]))
        # also a full set so that we can find the location of the 1
        diffSolsFULL <- abs(expandSolutions[i,] - expandSolutions[j,])
        #print(gsub("-1", "-", paste0(c(expandSolutions[i,], ",", expandSolutions[j,], ": ", diffSols), collapse = "")))
        # if there is only one difference between the masked variables
        if (sum(diffSols) == 1) {
          # find the location of the difference
          thisDiff <- paste0(diffSolsFULL, collapse="")
          diffLoc <- regexpr("1", thisDiff)[1]
          # if this is a binary variable
          if (varTypes[diffLoc] == "B") {
            #print(paste0(c(i, ",", j), collapse = ""))
            # turn it unspecified for both variables
            expandSolutions[i,diffLoc] <- -1
            expandSolutions[j,diffLoc] <- -1
            # mark this as a non-clean round
            clean = FALSE
            # break out of the inner loop
            break
            # if there is not just one difference
          } else if (varTypes[diffLoc] == "M") {
            # create an additional inner loop
            for (k in 1:nrow(expandSolutions)) {
              # if the inner loop is different from both i and j (the two existing loops)
              if (i != k && j != k)  {
                # if the difference between the differences between the two existing loops and the new loop is 1
                if (abs(sum(abs(expandSolutions[i,] - expandSolutions[k,])) - sum(abs(expandSolutions[j,] - expandSolutions[k,]))) == 1) {
                  # if all of the solutions are different
                  # if all of that is true...
                  if (expandSolutions[i,diffLoc] != expandSolutions[j,diffLoc] && expandSolutions[i,diffLoc] != expandSolutions[k,diffLoc] && expandSolutions[j,diffLoc] != expandSolutions[k,diffLoc]) {
                    #print(paste0(c(i, ",", j, ",", k), collapse = ""))
                    # unspecify this variable for all three
                    expandSolutions[i,diffLoc] <- -1
                    expandSolutions[j,diffLoc] <- -1
                    expandSolutions[k,diffLoc] <- -1
                    # mark as not clean
                    clean = FALSE
                    # break out of inner loop
                    break
                  } # end it solutions are different
                } # end if difference of differences == 1
              } # end if new inner loop value is different from other two loops
            } # end for k
          } else if (varTypes[diffLoc] == "C") { # end else
            for (k in 1:nrow(expandSolutions)) {
              for (m in 1:nrow(expandSolutions)) {
                # if the inner loop is different from both i and j (the two existing loops)
                if (i != k && j != k && i != j && i != m && j != m && k != m)  {
                  #print(paste0(c(expandSolutions[i,diffLoc], expandSolutions[j,diffLoc], expandSolutions[k,diffLoc], expandSolutions[m,diffLoc]), collapse = ", "))
                  # if the difference between the differences between the two existing loops and the new loop is 1
                  if (abs(sum(abs(expandSolutions[i,] - expandSolutions[m,])) - sum(abs(expandSolutions[j,] - expandSolutions[m,])) - sum(abs(expandSolutions[k,] - expandSolutions[m,]))) == 0) {
                    # if all of the solutions are different
                    # if all of that is true...
                    if (expandSolutions[i,diffLoc] != expandSolutions[j,diffLoc] && expandSolutions[i,diffLoc] != expandSolutions[k,diffLoc] && expandSolutions[j,diffLoc] != expandSolutions[k,diffLoc]) {
                      #print(paste0(c(i, ",", j, ",", k, ",", m), collapse = ""))
                      # unspecify this variable for all three
                      expandSolutions[i,diffLoc] <- -1
                      expandSolutions[j,diffLoc] <- -1
                      expandSolutions[k,diffLoc] <- -1
                      expandSolutions[m,diffLoc] <- -1
                      # mark as not clean
                      clean = FALSE
                      # break out of inner loop
                      break
                    } # end it solutions are different
                  } # end if difference of differences == 1
                } # end if new inner loop value is different from other two loops
              } # end for m
            } # end for k
          } 
          # if i and j are different, but are identical with the i mask, copy i to j
        } else if (i != j && identical(unlist(expandSolutions[i,which(expandSolutions[i,] >= 0)]),unlist(expandSolutions[j,which(expandSolutions[i,] >= 0)]))) {
          #print(paste0(c("s>> ", expandSolutions[i,], ",", expandSolutions[j,]), collapse = ""))
          clean = FALSE
          expandSolutions[j,] <- sapply(expandSolutions[i,], FUN = function(x) {x})
          break
        } 
      } # end for j
      # if we've broken out of j, break out of i
      if (clean == FALSE) {
        break
      }
    } # end for i
    # end the while loop if we've traversed all loops without incident
    if (clean == TRUE) {
      #print("-----------------") 
      cd = 0
    } else {
      # otherwise reduce any redundant or null solutions 
      expandSolutions <- unique(expandSolutions[which(rowSums(expandSolutions) > ncol(expandSolutions)*-1),])
    }
  }
  # transfer back to uniqueSolutions
  uniqueSolutions <- unique(expandSolutions)
  #
  # *** That's it for solution set up. Now let's generate some random cases ***
  #
  # let's create an empty data frame to hold the case data
  caseData <- data.frame(replicate(numVars,sample(0:1,numCases,rep=TRUE)))
  # and fill the data frame with randomly generated values
  caseData[,which(varTypes == "C")] <- replicate(length(which(varTypes == "C")),sample(-1:2,numCases,rep=TRUE))
  caseData[,which(varTypes == "M")] <- replicate(length(which(varTypes == "M")),sample(0:2,numCases,rep=TRUE))
  # make a data set that ensures equal distribution of solutions for some proportion of the trials if necessary
  if (distribute == TRUE) {
    for (i in 1:as.integer(numCases * distProp)) {
      thisSolutionRow <- i%%nrow(uniqueSolutions)+1
      # force caseData to take on the key values of solutions
      caseData[i,] <- sapply(uniqueSolutions[(thisSolutionRow),], FUN = function(x, i.row){sapply(seq_along(x), FUN = function(y, z, i.row){if (z[y] > -1) z[y] else caseData[i.row,y]}, z = x, i.row = i.row)}, i.row = i)
    }  
  }
  # name the columns
  colnames(caseData) <- LETTERS[1:numVars]
  # next we need to generate the outcomes so that they match the solutions
  outComes <- data.frame(matrix(NA, nrow = nrow(caseData), ncol = nrow(uniqueSolutions)))
  # for each of the solutions, create a column that tests each case against that solution
  for (i in 1:nrow(uniqueSolutions)) {
    # check each of the data sets againt the current solution, then record TRUE or FALSE
    outComes[,i] <- apply(caseData, 1, FUN = function(x,y,mask){identical(as.numeric(unlist(x[mask])),as.numeric(unlist(y[mask])))}, y = uniqueSolutions[i,], mask = which(uniqueSolutions[i,] >= 0))
  }
  # we're going to wait a little to output the final column of the case data file
  # because we may need to create clustering columns to the left of the outcome column
  # ok.
  # so let's convert what we've got to integers
  caseData <- as.data.frame(apply(caseData,1:2,FUN = function(x) {as.integer(x)}))
  # and now check if we're adding continuous variables for clustering
  if (length(which(varTypes == "C")) > 0) {
    # create a data frame to hold random numbers
    preScatter <- data.frame(matrix(NA, nrow = numCases, ncol = 2))
    # name the columns
    colnames(preScatter) <- c("C1", "C2")
    # create the first column. We'll make 4 clusters that border each other. three along
    # the hypotenuse of a right triangle and one in the right angle corner.
    # creates data in a subset relationship
    # which is also a pattern of data consistent with predictions at one end of a scale without
    # strong predictions at the other.
    # (this type of pattern is common in social science data and leads to consistent clustering solutions
    preScatter$C1 <- sapply(caseData[,which(varTypes == "C")], FUN = function(x){r = sample(0:1000,1,rep=TRUE); if (x >=0) (r*(1/3))/1000+(x*1/3) else (r*(1/3))/1000})
    # a temp variable to hold some random numbers.
    temprrr <- sample(0:1000,nrow(preScatter),rep=TRUE)
    # and another temp variable to hold the cases so that we can iterate through them with sapply()
    cases <- caseData[,which(varTypes == "C")]
    # well, this is a complicated function that keeps everything above the diagonal, and then 
    # create two clusters at the low end
    preScatter$C2 <- sapply(seq_along(temprrr), FUN = function(x){if (cases[x]>=1) (((1-preScatter$C1[x])*temprrr[x])/1000) + preScatter$C1[x] else ((((1-preScatter$C1[x])*temprrr[x]/2)/1000) + preScatter$C1[x] + ((1-preScatter$C1[x])/2) + (cases[x]*((1-preScatter$C1[x])/2)))})
    # make a list of clusters to output
    clusters <- caseData[,which(varTypes == "C")] + 1
    # add the created continuous data to the caseData data frame--first column
    caseData[which(varTypes == "C")] <- preScatter$C1
    # second column
    caseData[(which(varTypes == "C")+1)] <- preScatter$C2
    # rename the columns
    colnames(caseData)[(which(varTypes == "C")+1)] <- paste0(c(colnames(caseData)[which(varTypes == "C")],"2"),collapse="")
    colnames(caseData)[which(varTypes == "C")] <- paste0(c(colnames(caseData)[which(varTypes == "C")],"1"),collapse="")
  } else {
    # if we're not clustering, the clusters variable is NULL
    clusters <- NULL
  }
  # now, at last, we can create the OUT variable
  caseData$OUT <- apply(outComes,1,FUN = function(x){if (sum(sapply(x, FUN = function(y){if (y == TRUE) 1 else 0})) >= 1) 1 else 0})
  # clean path output to make comparison easier
  pathtext <- apply(uniqueSolutions, 1, FUN = function(x){sapply(seq_along(x), FUN= function(x, y, z){paste0(if (z[x] > -1) paste0(c(y[x],"{",z[x],"}"),collapse="") else NULL, collapse="")}, y = colnames(uniqueSolutions), z = x)})
  NApathtext <- apply(pathtext,1:2, FUN = function(x){if (x == "") NA else x})
  paths <- data.frame(sapply(apply(NApathtext, 2, FUN = function(x){y <- na.omit(x); paste0(y,collapse = "*")}), FUN = function(x){x}))
  colnames(paths) <- "PATHS"
  paths
  if (isTRUE(all.equal(varTypes, rep(varTypes[1], length(varTypes)))) && varTypes[1] == "B") {
    paths <- apply(paths, 1:2, FUN = function(x){p.var <- regmatches(x,gregexpr('[A-Z]', x, FALSE)); p.val <- regmatches(x,gregexpr('[0-9]', x, FALSE)); all.var <- sapply(seq_along(unlist(p.var)), FUN = function(x, var, val){if (val[x] == 0) tolower(var[x]) else var[x]}, var = unlist(p.var), val = unlist(p.val)); paste0(all.var, collapse="")})
    paths
  }
  # original paths
  oPathtext <- apply(originalSolutions, 1, FUN = function(x){sapply(seq_along(x), FUN= function(x, y, z){paste0(if (z[x] > -1) paste0(c(y[x],"{",z[x],"}"),collapse="") else NULL, collapse="")}, y = colnames(originalSolutions), z = x)})
  NAoPathtext <- apply(oPathtext,1:2, FUN = function(x){if (x == "") NA else x})
  oPaths <- data.frame(sapply(apply(NAoPathtext, 2, FUN = function(x){y <- na.omit(x); paste0(y,collapse = "*")}), FUN = function(x){x}))
  colnames(oPaths) <- "PATHS"
  oPaths
  if (isTRUE(all.equal(varTypes, rep(varTypes[1], length(varTypes)))) && varTypes[1] == "B") {
    oPaths <- apply(oPaths, 1:2, FUN = function(x){p.var <- regmatches(x,gregexpr('[A-Z]', x, FALSE)); p.val <- regmatches(x,gregexpr('[0-9]', x, FALSE)); all.var <- sapply(seq_along(unlist(p.var)), FUN = function(x, var, val){if (val[x] == 0) tolower(var[x]) else var[x]}, var = unlist(p.var), val = unlist(p.val)); paste0(all.var, collapse="")})
    oPaths
  }
  # later we'll put a subroutine here to create noisy data
  # but not yet
  if (noiseCases > 0) {
    for (i in 1:numVars) {
      if (varTypes[i] == "B") {
        caseData[(numCases+1):(numCases+noiseCases),i] <- sample(0:1,noiseCases,rep=TRUE)
      } else if (varTypes[i] == "M") {
        caseData[(numCases+1):(numCases+noiseCases),i] <- sample(0:2,noiseCases,rep=TRUE)
      } else if (varTypes[i] == "C") {
        caseData[(numCases+1):(numCases+noiseCases),i] <- sample(0:1000,noiseCases,rep=TRUE)/1000
        caseData[(numCases+1):(numCases+noiseCases),i+1] <- sample(0:1000,noiseCases,rep=TRUE)/1000
      }
      caseData[(numCases+1):(numCases+noiseCases),ncol(caseData)] <- sample(0:1,noiseCases,rep=TRUE)
    }
  }
  #
  #
  # construct a list of output variables
  fakeData <- list("paths" = paths, "clusters" = clusters, "comparator" = comparator, "redundantSolutions" = redundantSolutions, "uniqueSolutions" = uniqueSolutions, "caseData" = caseData, "allSolutions" = genSolutions, "opaths" = oPaths)
  # return the list
  return(fakeData)
}