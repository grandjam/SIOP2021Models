# Overview of compuational model
The R code contained in this folder replicates the computational model described in:

> Martell, R.F., Lane, D.M., & Emrich, C. (1996). Male-female differences: A computer simulation. *American Psychologist, 51,* 157-158.
 
The basic premise of the model is to demonstrate how a large degree of gender stratificaiton can emerge at the upper levels of an organizational hierarchy as a result of very small biases in the performance evaluation ratings provided to males versus females that are used to make promotion decisions.

## Model pseudocode
Martell et al. (1996) do not provide the exact pseudocode on which their model is based, but enough details are provided in the text to repliate the model. The basic steps carried out in the model are as follows:

1. Create hierarchical organization with k levels
2. Populate each organizational level with nk original employees
3. Assign each employee a gender such that nmale = nfemale 
4. Randomly assign each employee a performance evaluation score such that performancefemale ~ N(50,10) and performancemale ~ N(50,10) + bias
5. Randomly select TO% of employees to turnover from the organization
6. Determine if any open positions exist at level k and promote highest performing employees from level k-1 into openings
7. Fill open positions in lowest organizational level with new hires using procedure in Steps 3 and 4
8. If number of original employees > 0, return to Step 5
9. End

Each step of the pseudocode is copied into the R code for the computational model so that the user can see how these model steps were implemented. Furthermore, the R code for the model is extensively commented to describe what each line of code accomplishes.

## Running the model
Download and/or copy the entire R file to your computer. The variables listed under the section **Model Parameters** can be manipulated to evaluate their effect on model behavior. To run the model, adjust the parameter values to the desired level, select all the code, and press Run. Alternatively, you may run the code line by line to better understand what is happening within the code.
