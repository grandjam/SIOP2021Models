# Overview of compuational model
The R code contained in this folder replicates the computational model described in:

> Scullen, S.E., Bergey, P.K., Aiman-Smith, L. (2005). Forced distribution rating systems and the improvement of workforce potential: A baseline simulation. *Personnel Psychology, 58,* 1-32.
 
The basic premise of the model is to demonstrate how characteristics of forced distribution rating systems (i.e., firing a certain percentage of employees in an organization each year) interact with the the reliability and validity of a selection system, the selection ratio, and an organization's turnover rate to impact workforce performance potential.

## Model pseudocode
Scullen et al. (2005) provide the exact pseudocode on which their model is based in Table 1 of their paper. The basic steps carried out in the model are as follows:

1. Initialize time clock t = 0
2. Create 10 groups of 10 employees in each organization
3. Create a voluntary turnover schedule for each firm
4. Increment time clock t = t + 1
5. Invoke volutnary turnover and immediate replacement schedule for period t
6. Evaluate employees in accordance with Equation 3
7. Rank employees by Rating
8. Terminate employees with lowest Ratings
9. Create new applicants for each opening
10. Evaluate applicants in accordance with Equation 1
11. Rank applicants by application score
12. Hire applicants with highest application score
13. If t < 30, then go to Step 4
14. End

Each step of the pseudocode is copied into the R code for the computational model so that the user can see how these model steps were implemented. Furthermore, the R code for the model is extensively commented to describe what each line of code accomplishes.

## Running the model
Download and/or copy the entire R file to your computer. The variables listed under the section **Model Parameters** can be manipulated to evaluate their effect on model behavior. To run the model, adjust the parameter values to the desired level, select all the code, and press Run. Alternatively, you may run the code line by line to better understand what is happening within the code.
