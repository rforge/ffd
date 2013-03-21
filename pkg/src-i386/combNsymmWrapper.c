// Ian Kopacka
// 2010-05-21
//
// combNsymmWrapper(double *vector,  int *nVector, int *nGroups, long double *result)
//
// Recursive function used to compute the a posteriori alpha error for a given
// sample.

// Funktionsprototyp (Deklaration):
double combNsymm(double *vector, int nVector, int nGroups);

// Function called by R:
// Wrapper for the recursive function combNsymm
// In order to call a function in R using .C the C-function needs to be 
// of type void and all arguments need to be passed as pointers:
void combNsymmWrapper(double *vector,  int *nVector, int *nGroups, double *result){     
    *result = combNsymm(vector, *nVector, *nGroups); 
}

double combNsymm(double *vector, int nVector, int nGroups){
    double result = 0.0;
    int ii;    
    
    if (nVector >= nGroups){
        if (nGroups > 1){
            for (ii = nVector - 1; ii >= nGroups - 1; ii--){
                result += vector[ii]*combNsymm(vector, ii, nGroups-1);           
            }
        } else {
            for (ii = 0; ii < nVector; ii++){
               result += vector[ii];
            } 
        }
    }
    return(result);     
}
 
