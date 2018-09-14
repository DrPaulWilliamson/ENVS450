# tally() - function to create user-friendly (un)weighted tabulations of survey data
tally <- function(formula=NULL, format="count", data=NULL, na.rm=FALSE, cumsum=FALSE, margin="none",
                  dp=NULL, weights=NULL, flat=FALSE) {
  
  # A function, using a formula-driven interface, to create 1, 2 and 3-dimensional frequency tables,
  #   with options to
  # (a) Display in as user-friendly manner as possible (within limits!), including
  #     control of decimal places and avoidance of horizontal (vector) format for 1-D tables.
  # (b) Calculate counts or row/col/joint proportions or percentages
  # (c) Calculate row, col and joint marginal totals
  # (d) Calculate cumulative row or col counts, proportions or percentages
  # (e) Include/exclude missing (NA) values, using the na.rm option rather than xtabs non-standard
  #     exclude and na.action options (but also rather than a more user-friendly label
  #     such as useNA because na.rm is such a widespread option amongst R functions in general.
  # (f) Use weights
  # (f) Error trapping of common user mistakes
  #
  # Basic idea of a formula-based interface identical to that used in lm() etc inspired by the mosaic::tally function
  #
  # Author: Dr Paul Williamson, Centre for Spatial Demographics Research,
  #         Dept. of Geography & Planning, University of Liverpool
  #
  # Version: 4.0
  #
  # Date: 16th October 2016
  
  ## WISH LIST:
  ## 1. A print option which adds a header denoting whether a count, proportion or percentage is being displayed
  ## 2. If only one variable supplied, and percentages requested, automatically set margin to 'col' unless over-ridden by user
  ##    OR (Better?)
  ##    Re-engineer so that user specifies format as count, joint.pct, col.pct; row.pct;etc; and margin as row/col/joint,
  #     with margin defaults given user requested format that apply unless over-ridden (gives user max. flexibility)
  ## 3. Re-engineer to use xtabs() formula format as input: tally(Freq ~ X + Y + Z), with Freq=1 if not supplied
  ##    Rationale: lm(Y ~ X) is (arguably) more like tally(Freq ~ X) than tally(X ~ X)
  ##    More importantly, using the xtabs input in this way allows the name of the weights to be provided without
  ##    quotes, as per the other variables.
  
  ### SOLUTION FOR tally() USING WEIGHTS:
  
  #   1. Use base:::xtabs() as the basic table generator
  #   2. To make use of xtabs(), re-engineer formula supplied by user to tally() so that ALL variables are on the
  #   right-hand side of the formula, separated from each other by a +
  #     
  #     E.g. tally(AgeGroup ~ Sex)  becomes xtabs(~ AgeGroup ~ Sex)
  #   [Var on left in tally formula becomes first variable in list in xtabls formula ??]
  
  
  #   3. Add a weights=NULL paramater to tally().
  #   4. If weight = NULL, call xtabs as outlined above. If weight = var, then call to xtabs becomes
  #   xtabs(weight ~ x1 + x2)
  #   5. Error check to make sure that weight, if supplied, is a numeric vector; not a factor
  #   6. Error check to make sure that all other supplied variables are factors
  #   7. Offer the option to 'flatten' the final table using base:::ftable(). 
  #   If the option is reuested AND table has 3+ dimensions, then ftable(xtabs())
  
  #Check values of parameters supplied
  
  #(a) Check that a formula has been supplied.
  #Syntax below required to catch users supplying variable names in non-forumula format;
  #without the approach below, R throws an automatic error re. data object not existing,
  #but without explaining in simple terms what this means
  if (exists(as.character(substitute(formula)))==FALSE) 
    stop('Specification of required tabulation not given in formula format [most likley missing a ~]')
  
  #This line of code probably redundant due to the above...
  if (is.null(formula)) stop('No table requested: please specify a table using the formula= parameter')
  
  #(b) Check to see that a data.frame has been supplied
  if (is.null(data)) stop('No data supplied: please specify a dataset using the data= parameter')
  if (!is.data.frame(data)) stop ('Object supplied via the data= paramater is not a data.frame')
  
  
  #(c) Check validity of supplied formula
  
  #If user-supplied formula is NOT a formula, stop and warn user
  if (class(formula) != "formula") 
    stop('Specification of required tabulation not given in formula format [most likley missing a ~]')
  
  #If object IS a formula:
  #(which it is by definition if the code gets this far due to the error check above)
  
  #(c)[i] Check that the user-supplied variable names are valid (i.e. appear in the  user-supplied data.frame)
  #       At the same time check the validity of the supplied weights variable (if any)
  
  #Extract a list of the variables names included in the user-supplied formula
  var.names <- all.vars(formula)
  n.vars <- length(var.names)
  
  #Identify which variables (if any) are not present in the user-supplied data.frame
  #[add name of supplied weights variable, if any, to the list of var.names before testing]
  
  if (is.null(weights)==FALSE) {
    var.names2 <- c(var.names, weights)
  } else {
    var.names2 <- var.names
  }
  
  invalid.var.names <- which(var.names2 %in% names(data)==FALSE)
  
  #If there are 1+ invalid variables in the supplied formula + weights, halt and provide an appropriate error message
  if (length(invalid.var.names) > 0) {
    
    #Give user a list of valid options if fewer than 50 variables in supplied data.frame;
    #otherwise just tell them how to generate a list of valid variable names
    if (length(names(data))<50) {
      stop('\n',
           'The following variable(s) are missing from supplied data.frame:','\n', 
           paste0(var.names2[invalid.var.names],collapse=' '), '\n', '\n',
           paste0("Variables available for selection are:"),'\n',
           paste0(names(data), collapse= " "))
    } else {
      stop('The following variable names are not present in supplied data.frame:','\n', 
           paste0(var.names2[invalid.var.names],collapse=' '), '\n', '\n',
           paste0("Use the names() function to view valid options"))
    }
  }  
  
  #(d) Check that the supplied weights object (if any) is a numeric vector. 
  #    If supplied as a factor (e.g. because car:::recode() was used add weights to data.frame ), 
  #    then attempt to convert to a numeric vector
  
  if (is.null(weights)==FALSE) {
    if (is.factor(data[,weights])==TRUE) {
      data[,weights] <- as.numeric(as.character(data[,weights]))
    }
  }
  
  #(e) Convert the valid formula object (valid class; valid variables) into a xtabs formula format  
  
  #  Check how many terms are included in the formula (2 or 3+) and convert into xtabs formula format
  #  accordingly [~ Age + Sex instead of Age ~ Sex]
  #
  #  Note: a valid formula object must include a tilda (~)
  #  a valid formula object must also include a variable name AFTER the tilda
  #  Hence a formula of length 2 necessarily comprises: ~ var.name , and refers
  #  to a one-dimensional table.
  
  #      To force tally() to produce output in columns, rather than a simple horizontal vector of results,
  #      for one-dimensional tables it is necessary to add an additional column to the dataset, 'Count',
  #      with each entry comprising the single word 'All'; and to convert the formula from ~ var.name
  #      to var.name ~ Count
  
  if (n.vars == 1) {
    
    data <- cbind(data,Count=rep("All",nrow(data)))
    
    xtabs.formula <- 
      as.formula(
        
        paste(weights,
              "~",
              var.names,
              "+",
              "Count",
              collapse='' )
      )
  }
  
  if (n.vars > 1) {
    
    xtabs.formula <-
      as.formula(
        
        paste(weights,
              "~", 
              paste(var.names[1:length(var.names)-1], "+",collapse=''),
              var.names[length(var.names)]
        )    
      )
    
  }
  
  #(f) Check margin requests
  valid.margins <- c("row", "col", "joint", "none")
  if(margin %in% valid.margins) {
  } else {
    margin <- "none"
    warning("valid margins (case-sensitive) are: row, col, joint, none. The default setting 'none' is being used")
  }
  
  #(g) Check format requests
  valid.formats <- c("count","percent","proportion")
  if (format %in% valid.formats) {
    if ((format %in% c("percent","proportion")) & (margin == "none")) {
      warning("Requested format requires a margin; using default 'count' format instead")
    }
  } else {
    stop('valid formats (which are case-sensitive) are: count, percent, proportion')
  }
  
  
  #(h) Check na.rm requests
  valid.formats <- c(TRUE,FALSE)
  if(na.rm %in% valid.formats) {
    
    #useNA=!na.rm #useNA is the opposite of na.rm: (remove NAs = TRUE) == (useNA = FALSE)    
    #Given valid call to na.rm, convert request into xtabs format
    if (na.rm==TRUE) {
      
      #Set the xtabs exclude option to NA to remove any NAs from table
      exclude <- NA
      
    } else {
      
      #Set the xtabs exclude option to NULL to include any NAs from table
      exclude <- NULL
      
      #In addition, ensure that any variables in the table that include NA values have NA as a level;
      #otherwise xtabs won't include them
      #N.B. If NA already exists as a level, addNA() leaves this unchanged, so no harm invoking, except
      #a minor cost to memory and computational efficiency
      for (i in 1:n.vars) {
        if (any(is.na(data[,var.names[i]]))) data[,var.names[i]] <- addNA(data[,var.names[i]])
      }    
    }
    
  } else {
    
    stop('valid values for na.rm are: TRUE and FALSE')
    
  }
  
  #(i) Check the cumsum request
  if(cumsum %in% valid.formats) {
  } else {
    stop('valid values for cumsum are: TRUE and FALSE')
  }
  
  #(j) Check dp requests
  if (is.null(dp)) {
  } else {
    if (is.numeric(dp) == FALSE) warning('dp request ignored because incorrectly specified: please use integer numbers only')
  }
  
  #(k) Check flat requests
  if (flat %in% valid.formats) {
  } else {
    flat <- FALSE
    warning("Supplied paramter for flat= is not TRUE or FALSE; FALSE applied")
  }
  
  #Check cumsum & margin request combinations
  
  calc.addmargins <- FALSE
  if ((margin == "row") | (margin == "col") | (margin == "joint")) calc.addmargins <- TRUE
  
  calc.cumsum <- FALSE
  
  if (cumsum==TRUE) {
    if ((margin=="row") | (margin == "col")) {
      calc.cumsum <- TRUE
      calc.addmargins <- FALSE #No total margins on a cumsum table
    }
    if (margin=="joint") {
      calc.cumsum <- FALSE
      #calc.addmargins <- TRUE #already set as true above if margin == "joint"
      warning("Can't calculate cumulative sum for a joint distribution, so marginal totals returned")
    }
    if (margin=="none") {
      calc.cumsum <- FALSE
      warning('No margin specified, so cannot calculate cumulative sum')
    }
  }
  
  #If no user-supplied dp supplied, set to 0 for unweighted counts; set to 2 for weighted / percent / proportion tables
  if (is.null(dp)) {
    if ((format=="count") & (is.null(weights))) {
      dp <- 0 
    } else {
      dp <- 2
    }
  }
  
  #Create a function to calculate the cumulative sum of tables of 1, 2 or 3 dimensions, in
  #order to simplify later code.
  
  tally.cumsum <- function(table=table, margin=margin) {
    
    #(b) Calculate cumulative sums, if requested
    
    #if ((margin != "none") & (cumsum=TRUE)) warning('Cannot provide marginal totals and cumulative sums, so returning cumulative sums')
    
    #Find no.of dimensions in table
    n.dimensions <- length(dim(table))
    if (n.dimensions == 0) n.dimensions <- 1
    if (margin=="row") cumsum.margin <- 1
    if (margin=="col") cumsum.margin <- 2
    if (margin=="joint") cumsum.margin <- NA
    if (margin == "none") cumsum.margin <- NA
    
    if (is.na(cumsum.margin)) {
      if (margin == "joint") warning('Cannot provide cumulative totals for joint probabilites, so returning table margins instead')
      if (margin == "none") warning('No margin specified, so cannot provide cumulative totals')
    } else {
      #Find size of third dimension (if any)
      if (n.dimensions == 3) {
        n.dim3.categories <- length(dimnames(table)[[3]])
        for (i in 1:n.dim3.categories) {
          tmp <- apply(table[,,i], MARGIN=cumsum.margin, cumsum)
          if (margin == "row") {
            table[,,i] <- t(tmp)
          } else if (margin == "col") {
            table[,,i] <- tmp
          }
        }
      } else if (n.dimensions == 2) {
        tmp <- apply(table, MARGIN=cumsum.margin, cumsum)
        if (margin == "row") {
          table <- t(tmp) 
        } else if (margin == "col") {
          table <- tmp
        }
      } else if (n.dimensions == 1) {
        table <- cumsum(table)  
      }
    } #if / else cumsum required
    
    return(table)
    
  }
  
  ###
  ### Create the basic frequency count table
  ### which forms the basis for all other table variants
  ###
  
  table <- xtabs(xtabs.formula, data= data, exclude= exclude)
  
  #Implement required format: count; (row or col) percentages or proportions
  #Table already in count format, so action only needed if another format type is required
  
  #(a) Type of proportion/percentage is specified by the user-supplied value of 'margins':
  #    row, col or joint, and needs to be converted into the required input value
  #    for prop.table() and addmargins()
  #
  #   By design 'margins' is also used to specify which margins are added (row, col or both). 
  #   This ensure that proporition or % table come supplied with the relevant
  #   row, column or joint marginal totals, since these marginal totals clearly communicate
  #   what type of table is being viewed.
  
  if (margin == "row") {
    prop.table.margin <- 1
    addmargins.margin <- 2
  } else if (margin == "col") {
    prop.table.margin <- 2
    addmargins.margin <- 1
  } else if (margin == "joint") {
    prop.table.margin <- NULL
    addmargins.margin <- c(1,2)
  } else {
    prop.table.margin <- NA #prop.table() will not be called if margin is not one of row, col or joint
    addmargins.margin <- NA #addmargins() will not be called if margin is not one of row, col or joint
  }
  
  
  #(c) If a marginal total has been requested
  
  if ((margin != "none")) { #If not none...
    
    #(d) Use the requested margin to generate the relevant type of proportion/percentage table (if any)
    if ((format=="proportion") |  (format=="percent")) {
      
      #If table is 2D... [**or 1D??**]
      if (length(dim(table))<3) {
        table <- prop.table(table, margin=prop.table.margin)
      }
      
      #If table id 3D, convert the 'inner' 2D tables to proportions one at a time
      if (length(dim(table))==3) {    
        #Convert inner table to proportions along specified margin
        for (i in (1:dim(table)[3])) {
          table[,,i] <- prop.table(table[,,i], margin=prop.table.margin)
        } #next i
      } #if length...
    } #if format...
    
    
    #(e) If percentages are required...
    if (format=="percent") table <- table * 100
    
    if (calc.cumsum==TRUE) {
      
      #if ((margin != "none") & (cumsum==TRUE)) warning('Cannot provide marginal totals and cumulative sums, so returning cumulative sums')
      table <- tally.cumsum(margin=margin, table=table)
      
    }
    
    #(f) Implement required marginal totals: row or col or joint (if required)
    
    if (calc.addmargins == TRUE) {
      
      table <- addmargins(table, margin=addmargins.margin)
      
      
      #(g) Change default name for total column from Sum to Total
      #    [if a total column has been added]
      #    [Only needs to be done for the first two dimensions,
      #    because marginal totals only calculated for the first
      #    two dimensions]
      if ((margin=="col") | (margin == "joint")) {
        dimnames(table)[[1]][length(dimnames(table)[[1]])] <- "Total"  #Dim 1 = cols
      }
      if ((margin=="row") | (margin == "joint")) {
        dimnames(table)[[2]][length(dimnames(table)[[2]])] <- "Total"  #Dim 2 = rows 
      }
    } #if cacl.addmargins == TRUE  
    
  } #If a margin is required
  
  if (flat==FALSE) {
    return( as.table(round(table,digits=dp)) )
  } else {
    return( ftable(round(table,digits=dp)) )
  }
  
} #end function