// Ian Kopacka
// 2010-05-27
//
// combNWrapper(double *alphaVec, int *nAlpha, int *nPot, int *potVec, 
//     int *indVec, double *result)
//
// Recursive function used to compute the a posteriori alpha error for a given
// sample.

// Funktionsprototyp (Deklaration):
double combNsymmIndex(double *alphaVec, int maxInd, int indVec[], int pot);
double combNDecompSymmIndex(double *alphaVec, int maxInd, int indVec[], int pot, int *nAlpha, int nPot, int *potVec);
double combN(double *alphaVec, int *nAlpha, int nPot, int *potVec, int indVec[]);

// Function called by R:
// Wrapper for the recursive function combNsymm
// In order to call a function in R using .C the C-function needs to be 
// of type void and all arguments need to be passed as pointers:
void combNWrapper(double *alphaVec, int *nAlpha, int *nPot, int *potVec, 
    int *indVec, double *result){
    *result = combN(alphaVec, nAlpha, *nPot, potVec, indVec);     
}
    
      
//===========================================================================//
//===========================================================================//
// Subroutines:
double combN(double *alphaVec, int *nAlpha, int nPot, int *potVec, int indVec[]){
    double result = 0.0;
    
    if (nPot > 1){
        result = combNDecompSymmIndex(alphaVec, *nAlpha, indVec, potVec[nPot-1], nAlpha, nPot, potVec);    
    } else {
        result = combNsymmIndex(alphaVec, *nAlpha, indVec, potVec[0]);    
    }      
    return(result); 
}


// Entspricht combNsymm2
// combNsymm mit Indexvektor
double combNsymmIndex(double *alphaVec, int maxInd, int indVec[], 
    int pot){
        
    double result = 0.0;
    int ii;
    int sumInd = 0;
    
    // Anzahl der zur Verfügung stehenden Elementen:
    for (ii = 0; ii < maxInd; ii++){
        sumInd += indVec[ii];   
    }
    
    if (sumInd >= pot){
        if (pot > 1){
            for (ii = (pot-1); ii < maxInd; ii++){
                if (indVec[ii]){
                    result += alphaVec[ii] * combNsymmIndex(alphaVec, ii, 
                        indVec, pot-1);              
                }                
            }
        } else {
            for (ii = 0; ii < maxInd; ii++){
                result += alphaVec[ii] * indVec[ii];    
            }   
        }
    }
    return(result);
}


// Entspricht combNsymm3
// combNasymm mit blockweise Zerlegung in symmetrische Einheiten
double combNDecompSymmIndex(double *alphaVec, int maxInd, int indVec[], 
    int pot, int *nAlpha, int nPot, int *potVec){
        
    double result = 0.0;   
    int ii;
    int sumInd = 0;    
    
    // Anzahl der zur Verfügung stehenden Elementen:
    for (ii = 0; ii < maxInd; ii++){
        sumInd += indVec[ii];   
    }
    
    if (sumInd >= pot){
        if (pot > 1){
            for (ii = (pot-1); ii < maxInd; ii++){
                if (indVec[ii]){    
                    indVec[ii] = 0;
                    result += alphaVec[(nPot-1) * nAlpha[0] + ii] * combNDecompSymmIndex(alphaVec, ii, 
                        indVec, pot-1, nAlpha, nPot, potVec); 
                    indVec[ii] = 1; 
                }
            }                       
        } else { 
            for (ii = 0; ii < maxInd; ii++){
                if (indVec[ii]){    
                    indVec[ii] = 0;
                    result += alphaVec[(nPot-1) * nAlpha[0] + ii] * combN(alphaVec,  
                        nAlpha, nPot-1, potVec, indVec); 
                    indVec[ii] = 1; 
                }
            }          
        
        }    
    }
    return(result);
}
