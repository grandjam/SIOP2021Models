# CITATION FOR ORIGINAL MODEL:
# Scullen, S.E., Bergey, P.K., Aiman-Smith, L. (2005). Forced distribution rating systems and the improvement of workforce potential: A baseline simulation. Personnel Psychology, 58, 1-32.

# Steps for the model pseudocode are indicated by [#] in the comments below (i.e., [1] corresponds to the first step of the model pseudocode)

####################
# MODEL PARAMETERS #
####################
time = 30 # number of years/time steps in simulation
org = 100 # number of organizations to simulate under the parameters listed below
grp = 10 # number of groups
emp = 10 # number of employees per group

fire = .10 # % of employees to fire from each org each year (attempts to fire employees equally across groups) [0-1]
rel = .70 # interrater reliability of true performance ratings [0-1]
val = .30 # criterion validity coefficient for selection system [-1,1]
sr = .10 # selection ratio [0-1]
volTurn = .10 # voluntary turnover rate [0-1]

###################
# MODEL FUNCTIONS #
###################
# Function for hiring a new applicant for an open position in the organization from an applicant pool
# Arguments:
# (1) selectionRatio = number of people to hire divided by the number of applicants; should be equal to sr variable (line 15)
# (2) validity = validity of selection system; should be equal to val variable (line 14)
# Returns:
# (1) Vector containing two values (performance potential, application score) for single applicant with highest application score (i.e., the person hired from the applicant pool for the opening)
appPoolHires <- function(selectionRatio, validity) {
  ## Identify number of applicants for a single position
  size = 1/selectionRatio
  ## Create empty matrix to hold applicant pool data; column 1 = performance potential, column 2 = applicant score on selection system
  app = matrix(0, nrow = size, ncol = 2)
  ## Randomly sample a true performance potential for all applicants in pool
  app[,1] = rnorm(size, 0, 1)
  ## Randomly sample error in application score for all applicants in pool
  error = rnorm(size, 0, 1)
  ## Compute score on selection system for all applicants in pool according to Equation 1 in Scullen et al
  app[,2] = validity * app[,1] + (sqrt(1-(validity^2)) * error)
  return(app[which.max(app[,2]),])
}

##########################################
# INITIALIZE INPUT DATA AND MODEL OUTPUT #
##########################################
# [1] Initialize time clock i = 0
# [2] Create grp number of groups with emp number of employees
# Create data frame matrix to hold data on all employee level variables
empMat = data.frame("orgID" = rep(1:org, each = emp*grp), # unique organization ID
                    "grpID" = rep(rep(1:grp, each = emp), times = grp), # group ID within each organization
                    "empID" = rep(1:emp, times = emp*grp), # employee ID within each group
                    "potential" = NA, # true performance potential of employee
                    "appScore" = NA, # employee's score on the selection system when they were hired
                    "rating" = NA, # performance evaluation rating received by employee
                    "TO" = F, # variable indicating whether employees is turning over/departing from level (T) or not (F)
                    "fire" = F) # variable indicating whether employees should be fired (T) or not (F) based on performance evaluation
# Creates potential and appScore for all employees initially in the organization by running appPoolHires function. Note that this function is run using the built-in "replicate" function in R, which repeats a function a given number of times 
empMat[,c("potential", "appScore")] <- t(replicate(nrow(empMat), appPoolHires(selectionRatio = sr, validity = val)))

# Create data frame to hold organizational level data on performance potential over time; # of rows = time, # of cols = organization
output = as.data.frame(matrix(0, nrow = time, ncol = org))
colnames(output) = paste0("Org", 1:org)

# [3] Create voluntary turnover schedule for each organization
# Creates a matrix with # of rows = time, # of cols = organization; data in matrix indicate the number of employees that will be turning over from an organization at a given time point
volTurnSchd = matrix(rpois(org*time, (volTurn*grp*emp)), nrow = time, ncol = org)

#########
# MODEL #
#########
# START OF LOOP FOR RUNNING THE MAIN MODEL PROCESS
for (i in 1:time) {
  # [4] Increment time clock i = i+1
  # [5] Invoke voluntary turnover with immediate replacement scheduled for time period
  for (j in 1:org) { # Loop through each organization and perform the actions below
    volTurnNdx = sample(1:(emp*grp), volTurnSchd[i,j]) # Randomly select which employees from the organization will be turning over
    empMat$TO[empMat$orgID == j][volTurnNdx] <- T # Flag which employees will be turning over in empMat
    empMat[empMat$orgID == j & empMat$TO == T, c("potential", "appScore")] <- t(replicate(length(volTurnNdx), appPoolHires(selectionRatio = sr, validity = val))) # Replace employees flagged for turnover with a new hire by running appPoolHires function
  }
  empMat$TO = F # Reset TO flag variable for all employees

  # [6] Evaluate employees using Equation 3 from Scullen et al paper
  empMat$rating = empMat$potential * sqrt(rel) + (rnorm(org*grp*emp) * sqrt(1 - rel))

  # Compute/record org performance potential by averaging performance potential of all empoloyees in the org
  output[i,] = aggregate(empMat$potential, by = list(empMat$orgID), mean)[,2]
  
  # [7] Rank employees by rating
  empMat = empMat[order(empMat$orgID, empMat$grpID, empMat$rating),]

  # [8] Terminate employees with lowest Ratings
  firePerGrp = fire*grp # Identify number of employees to fire within each group based on fire parameter
  if (firePerGrp %% 1 == 0) { # Check if firePerGp works out so that the same number of employees can be fired from each group
    for (j in 1:org) { # Loop through each org
      for (k in 1:grp) { # Loop through each group within each org
        empMat$fire[empMat$orgID == j & empMat$grpID == k][1:firePerGrp] = T # Flag firePerGrp number of employees from each group and flag for firing
      }
    }
  } else { # If firePerGrp does not work out so that the same number of employees can be fired from each group
    allGrpFire <- floor(firePerGrp) # Determine number of employees that will be fired from ALL groups in an organization
    remFire <- firePerGrp - allGrpFire # Determine remaining number of employees to fire
    for (j in 1:org) { # Loop through each org
      remFireGrpNdx <- sample(1:grp, (floor(remFire*grp))) # Randomly sample groups to determine which groups will fire additional employees in a given year
      for (k in 1:grp) { # Loop through each group
        if (k %in% remFireGrpNdx) { # Check whether group k has been selected to fire additional employees this time period
          empMat$fire[empMat$orgID == j & empMat$grpID == k][1:(allGrpFire+1)] = T # If true, fire allGrpFire + 1 employees
        } else {
          empMat$fire[empMat$orgID == j & empMat$grpID == k][1:allGrpFire] = T # If not true, fire allGrpFire employees
        }
      }
    }
  }
  # [9] Create new applicants for each opening
  # [10] Evaluate applicants using Equation 1
  # [11] Rank applicants by AppScore
  # [12] Hire applicants with highest AppScores
  # Replace all employees flagged as fired with new employee by running appPoolHires function
  empMat[empMat$fire == T, c("potential", "appScore")] <- t(replicate(sum(empMat$fire), appPoolHires(selectionRatio = sr, validity = val)))
  empMat$fire = F # Reset fire flag variable for all employees
}

###############
# DATA OUTPUT #
###############
wrkPtl <- rowMeans(output) # Compute average workforce performance potential across all organizations at each time point
plot(1:time, wrkPtl, type="b", lwd=1.5, xlab = "Year", ylab = "Average Performance Potential")