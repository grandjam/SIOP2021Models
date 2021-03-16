# CITATION FOR ORIGINAL MODEL:
# Martell, R.F., Lane, D.M., & Emrich, C. (1996). Male-female differences: A computer simulation. American Psychologist, 51, 157-158.

# Steps for the model pseudocode are indicated by [#] in the comments below (i.e., [1] corresponds to the first step of the model pseudocode)

####################
# MODEL PARAMETERS #
####################
nPerLevel = c(500,350,200,150,100,75,40,10) # number of employees per organizational level; from Table 1 of Martell et al.
n = sum(nPerLevel) # total number of employees
TOrate = .15 # turnover rate; percentage of employees [0-1] that depart from organization each year
bias = 4.58 # amount of bias added to performance evaluation ratings of male employees

nSims = 20 # number of times to run the model under the parameters above


###################
# MODEL FUNCTIONS #
###################
# Function for promoting employees up a single level of the organizational hierarchy
# Arguments:
# (1) orgMatProm = subset of orgMat data frame containing only the employees in levels j and j-1 of the organizational hierarchy (i.e., employees in level 8 & 7)
# Returns:
# (1) orgMatProm = subset of orgMat data frame in which highest performing individuals from level j-1 have been promoted to level j
promote <- function(orgMatProm) {
  ## Identify total number of employees who turned over in org level j
  numTO = sum(orgMatProm$TO[orgMatProm$level == max(orgMatProm$level)])
  ## Rank order employees in org level j-1 from best to worst and flag which employees from this level will be selected for promotion
  orgMatProm$promote[order(orgMat$perf[orgMat$level == j-1], decreasing = T)[1:numTO]] <- 1
  ## Replace the data for employees from org level j who are turning over with the data for employees for org level j-1 that have been flagged for promotion
  orgMatProm[orgMatProm$level == max(orgMatProm$level) & orgMatProm$TO == T, c("original","gender","perf")] <- orgMatProm[orgMatProm$level == min(orgMatProm$level) & orgMatProm$promote == T, c("original","gender","perf")]
  ## Flag the employees who were promoted as "turning over" so that they will be replaced later
  orgMatProm[orgMatProm$level == min(orgMatProm$level) & orgMatProm$promote == 1, "TO"] <- T
  return(orgMatProm)
}

# Function for hiring new employees into lowest level of the organizational hierarchy
# Arguments:
# (1) orgMatHire = subset of orgMat data frame containing only those employees in level 1 (lowest level) and who were flagged as having departed/turned over (i.e., orgMat$TO == 1)
# Returns:
# (1) orgMatHire = subset of orgMat data frame containing new hires for level 1
newHire <- function(orgMatHire) {
  ## Indicate that all newly hired employees are not initial/original employees
  orgMatHire$original = 0
  ## Randomly sample the gender of all new hires
  orgMatHire$gender = sample(c(0,1), nrow(orgMatHire), replace = T)
  ## Randomly sample the performance of all new hires
  orgMatHire$perf = rnorm(nrow(orgMatHire), 50, 10)
  ## Add bias to the performance for new male hires
  orgMatHire$perf[orgMatHire$gender == 1] = orgMatHire$perf[orgMatHire$gender == 1] + bias
  return(orgMatHire)
}


##########################################
# INITIALIZE INPUT DATA AND MODEL OUTPUT #
##########################################
# Create data frame to hold the results from each simulation run; each column of the data frame records the number of female employees at each level of the organizational hierarchy at the conclusion of the simulation
simResults = data.frame("run" = 1:nSims,
                        "l1Female" = NA,
                        "l2Female" = NA,
                        "l3Female" = NA,
                        "l4Female" = NA,
                        "l5Female" = NA,
                        "l6Female" = NA,
                        "l7Female" = NA,
                        "l8Female" = NA)

# START OF THE LOOP FOR RUNNING MULTIPLE ORGANIZATIONS UNDER THE SAME PARAMETER SETTINGS. IF YOU ONLY WANT TO RUN 1 SIMULATION/ORGANIZATION UNDER A GIVEN SET OF PARAMETERS, SET nSims = 1 ON LINE 14
for (i in 1:nSims) {
  # [1] Create hierarchical organization with k levels
  # [2] Populate each organizational level with nk original employees
  # [3] Assign each employee a gender such that nmale = nfemale
  # [4] Randomly assign each employee a performance evaluation score such that performance of females ~ N(50,10) and performance of males ~ N(50,10) + bias
  # Create data frame for holding the data on all employees in a single organization
  orgMat = data.frame("empID" = 1:n, # unique employee ID
                      "level" = c(rep(1,nPerLevel[1]), # Level ID for each employee
                                  rep(2,nPerLevel[2]), 
                                  rep(3,nPerLevel[3]), 
                                  rep(4,nPerLevel[4]), 
                                  rep(5,nPerLevel[5]), 
                                  rep(6,nPerLevel[6]), 
                                  rep(7,nPerLevel[7]), 
                                  rep(8,nPerLevel[8])),
                      "original" = T, # Indicates whether employee is an initial/original employee (T) or a new hire (F); initially, all employees will be original
                      "gender" = rep(c(0,1), length.out = n), # Identify whether employees are female (0) or male (1); initially, the number of males and females will be (approximately) equal within each level
                      "perf" = rnorm(n, 50, 10), # Randomly sample employee performance level from a normal distribution
                      "promote" = F, # Variable indicating whether employee is being promoted (T) or not (F)
                      "TO" = F) # Variable indicating whether employees is turning over/departing from level (T) or not (F)
  orgMat$perf[orgMat$gender == 1] <- orgMat$perf[orgMat$gender == 1] + bias # Add "bias points" to male employees' performance

  
#########
# MODEL #
#########
  # START OF LOOP FOR RUNNING THE MAIN MODEL PROCESS
  while (any(orgMat$original == T)) { # Check whether any original/initial employees still remain. If true, continue code; if false, simluation ends
    # [5] Randomly select TO% of employees to turnover from the organization
    toVec = sample(1:n, as.integer(TOrate*n)) # Employees chosen to turnover are stored in a vector (toVec)
    orgMat$TO[toVec] = 1 # Change TO variable for employees chosen to turnover to 1
    orgMat[toVec, c("original","gender","perf")] = NA # Zero out the original, gender, and performance variables of departing employees
    
    # [6] Determine if any open positions exist at level k and promote highest performing employees from level k-1 into openings
    for (j in length(nPerLevel):2) { # Create an iterator vector that goes from highest org level to org level 2 (e.g., 8, 7, 6, 5, 4, 3, 2)
      if (any(orgMat$TO[orgMat$level == j] == T)) { # Check whether any employees turned over/departed from org level j. If true, execute code below; if false, go to next org level
        orgMat[orgMat$level %in% c(j,j-1),] <- promote(orgMatProm = orgMat[orgMat$level %in% c(j,j-1),]) # Select employees in org level j and j-1 and run the "promote" function
      }
    }
    orgMat[orgMat$level == 1 & orgMat$TO == T,] <- newHire(orgMatHire = orgMat[orgMat$level == 1 & orgMat$TO == T,]) # Hire new employees into level 1 by running the "newHire" funciton 
    orgMat$promote = orgMat$TO = F # Reset the promotion and turnover variables for the next iteration
  } 
  # END OF LOOP FOR RUNNING THE MAIN MODEL PROCESS
  simResults[i,] <- c(i,(nPerLevel - aggregate(orgMat$gender, by = list(orgMat$level), sum)[,2])/nPerLevel) # Record proportion of females at each org level at the end of the simulation run
}


###############
# DATA OUTPUT #
###############
plot(1:length(nPerLevel), colMeans(simResults[2:9]), "b", ylim = c(0,1), xlab = "Organizational Level", ylab = "Proportion of females")
