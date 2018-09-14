group.data <- function(data=data, breaks=breaks, output.dp=NULL, include.lowest=TRUE, integer.dp= 6,
                       mid.point=FALSE) {
  
  # This is a wrapper for the cut() function designed to provide more immediately user-friendly
  # (if less scientifically accurate) category labels.
  # cut() provides category labels in "(LB,UB]" format, with each LB being the same as its
  # preceding UB, since (LB,UB] means LB < x <= UB
  # For novice analysts (and non-expert consumers of research results), the unfamiliar
  # mathematical representation of bounds is problematic,
  # Their preferred output format is "LB-UB", using LBs that do not overlap their preceding
  # UBs. (E.g. 16-18; 19-21 etc.)
  # This wrapper uses cut() to categorise the supplied data, but then amends the category
  # labels returned by cut() along the lines outlined above.
  # For integer data, the revised category labels lose no information.
  # For real data, the conversion can leave unclear which category a fractional sits in if
  # the data are measured to greater precision than the class boundaries.
  # E.g. Does 0.71 fall in category 0.6-0.7 or 0.8-0.9.
  # For the intended user base and results audience, this loss in scientific precision
  # is, hopefully, more than compensated for by  more immediately accessible class boundaries.
  
  # Temporary parameter declarations for function testing purposes
  # data= qlfs$Age
  # breaks <- c(16, 29, 64, Inf)
  # groups <- 5
  # breaks <- quantile(qlfs$Age, probs=seq(from= 0, to= 1, by= 1/groups), na.rm=TRUE)
  # data <- qlfs$weights.num
  # breaks <- 5
  # output.dp <- 1
  # include.lowest=TRUE
  # integer.dp=6
  
  #Check to see if, to all intents and purposes, data are actually integer
  fractional.values <- which((data %% 1) > 10^-integer.dp)
  
  #data <- as.integer(data) doesn't stop cut() returning fractional value labels; therefore
  #need to auto-detect of values to be cut are integer and take appropriate action as a result
  #[i.e. produce class labels using integer-based class boundaries]
  if (length(fractional.values)==0)  {
    integer.values <- TRUE 
  } else {
    integer.values <- FALSE
  }
  
  #Find size of max. integer (i.e. places above the decimal point)
  integer.places <- nchar(as.character(floor(max(data, na.rm=TRUE))))
  
  #If output.dp supplied by user is >0, but integer data supplied, over-ride and set to 0dp;
  #and warn user that this has been done
  if ((is.null(output.dp)==FALSE) & (integer.values==TRUE)) {
    if (output.dp > 0) {
      output.dp <- 0
      warning('For integer data values, only integer class bounds will be returned, so output.dp reset to 0 ')
    }
  }
  
  #If output.dp not supplied by user, set to default value
  #[0 for integer data; 0.1 for 10^2 and above; 0.01 for 10^1; 0.001 for 10^0
  if (is.null(output.dp)==TRUE) {
    if (integer.values==TRUE) {
      output.dp <- 0
    } else {
      if (integer.places >= 3) output.dp <- 1
      if (integer.places == 2) output.dp <- 2
      if (integer.places == 1) output.dp <- 3
    }
  }
  
  
  #Set dig.lab to no. of places in supplied data (+ requested output.dp, if any) + 1
  #This:
  #(a) avoids the danger of cut() returning values in scientific notation (e.g. 2e+01),
  #which it will if dig.lab < places
  #[Obviously not great if handling very large values, but this is unlikely to
  #be an issue for social survey analysis]
  #(b) ensures a precision one dp greater than that used in final labelling,
  #allowing use of strategy of LB = rounded cut() UB labels; LB = UB - 10^output.dp
  
  if (is.null(output.dp)==TRUE) {
    dig.lab <- integer.places +1
  } else {
    dig.lab <- integer.places + output.dp + 1
  }
  
  cut.result <- cut(data, breaks, include.lowest= include.lowest, dig.lab=dig.lab)
  #levels(cut.result)
  
  #Extract original factors labels returned by cut() function
  cut.labels <- levels(cut.result)
  
  #Convert into a numeric vector of upper and lower-bound values
  
  #Strip out the opening and closing brackets
  char.labels <- substr(cut.labels, start=2, stop=nchar(cut.labels)-1)
  char.labels
  
  #Split each lower-upper bound pair into separate character values
  char.labels2 <- unlist(strsplit(char.labels, split=","))
  char.labels2
  
  
  #Split each lower and upper bound value into its integer and fractional parts;
  #then retain the fractional part as a character string
  #Note:
  #  (a) Need to use a loop approach rather than unlist() because unlist() doesn't return a value (0; NA; NULL)
  #      when there is no fractional part
  #  (b) Also need to adopt this approach because as.numeric(char.string) returned 0.1999999 for "0.2" when part of
  #      a longer string of character-based numbers, whereas what is needed here is a precise count of the length
  #      of the fractional part of each bound, as an input to the bounds tidying-up process
  
  #Split each lower and upper bound value into its integer and fractional parts;  
  tmp <- strsplit(char.labels2, '\\.') #needs the \\ before the period because the period is a special character
  
  #Extract fractional elements only
  fractional.part <- NULL
  for (i in 1:length(tmp)) {
    fractional.part[i] <- tmp[[i]][2]
    if (is.na(tmp[[i]][2])) fractional.part[i] <- ""
  }
  
  #Convert lower and upper bounds from cut() into numeric format
  num.labels <- as.numeric(char.labels2)
  
  #Find no. of classes [same as length(levels(cut.result))]
  n <- length(num.labels)/2
  
  #Split the numeric vector of lower and upper bound values into two vectors:
  #one of lower bound values; one of upper bound values
  lower.label <- num.labels[seq(1,n*2,2)]
  upper.label <- num.labels[seq(2,n*2,2)]
  
  #cut() over-inflates the top UB by +.001 x range. This can therefore lead to
  #misleading UBs: e.g. max value = 99, which cut inflates to >= 100, and hence
  #will never be rounded down to 99, regardless of selected output.dp
  #The same problem applies at the bottom LB, which cut() deflates by -0.001 x range
  #SOLUTION: 
  #(a) fix top UB at max bound specified by user (if any); else at max data value
  #(b) fix bottom LB at min bound specified by user (if any); else at min data value
  
  #If breaks > 1, then bounds in use are user-specified
  if (length(breaks)>1) {
    upper.label[n] <- round(breaks[n+1], output.dp)
    lower.label[1] <- round(breaks[1], output.dp)
  } else{
    upper.label[n] <- round(max(data, na.rm=TRUE), output.dp)
    lower.label[1] <- round(min(data, na.rm=TRUE), output.dp)
  }
  
  #Set remaining UBs. 
  #For both fractions and integers, round UBs down to required output dp
  #(e.g. 0.748 -> 0.74; 33.7 -> 33 etc).
  #Rationale: For integers, 33.7 includes data value 33, but not value 34.
  #Hence rounding down ensures that data value <= UB.
  #Case for rounding of fractions is less clear-cut, but same principal applied,
  #since 0.748 includes data value 0.74, but not value 0.75
  #
  #N.B.This strategy only works if output.dp is set to 0 for integer data,
  #over-ruling any user-requested dp.
  #Code below rounds down to output.dp
  
  upper.label[1:n-1] <- floor(upper.label[1:n-1]*10^output.dp) * 10^-output.dp
  
  
  #Adjust the LB to be 10^-output.dp less than their preceding UB
  #(excepting the bottom LB, already set above)
  lower.label[2:n] <- upper.label[1:n-1]+(10^-output.dp)
  
  #Adding 10^-output.dp to UBx to create LBx+1 runs the risk of 
  #LBx+1 ending up greater than UBx+1 if the class bounds are over-rounded.
  #Check for this and provide a user-warning if appropriate.
  if (any((lower.label > upper.label)==TRUE)) {
    warning("One or more lower-bounds exceeds it's class upper bound.","\n",
            "This is due to excessive rounding of the class bounds. Use output.dp to control")
  }
  
  
  #Create character versions of the 'tidied-up' boundary values
  link <- "-"
  tidy.labels <- NULL
  
  #Use a loop here, otherwise paste adds undesired padding: E.g. _1-_4 to match 21-34 instead of 1-4 and 21-34
  #Rounding arguably unnecessary here, but guards against code above (attempting) to
  #round diff. parts of the lower/upper.label vectors in diff parts of the code
  for (i in 1:n) {
    tidy.labels[i] <- paste(format(round(lower.label[i], output.dp), nsmall=output.dp),
                            link,
                            format(round(upper.label[i], output.dp), nsmall=output.dp),
                            sep="")
  }  
  
  #Check to see if the upper bound is infinite. (Can only happen via user-specified breaks)
  #If so, replace with top-coding. E.g. 20+ instead of 20-Inf
  if (breaks[length(breaks)]==Inf) {
    tidy.labels[n] <- paste(lower.label[n],"+",sep="")
  }
  
  #Check to see if user has requested interval mid-points to be used as interval labels instead of
  #class boundaries. Convert labels to interval mid-points if required.
  if (mid.point == TRUE) {
    for (i in 1:n) {
      tidy.labels[i] <- (upper.label[i] + lower.label[i]) / 2
    }
    
  }
  
  levels(cut.result) <- tidy.labels
  return(cut.result)
  
}
