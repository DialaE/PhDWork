#############################################################################################################
###################################FUNCTION#################################################################

RandomVector <- function(n, vproba) {
#! @brief give a vector of 0 and 1 randomly.
#! @param n : num of inputs
#! @param vproba : la probabilité de garder un point
#! @result a vector of n values of 0 or 1. Attention, il est impossible que toutes les
#!  valeurs soient nulles

  proba = as.real(vproba);
  if (is.na(proba) || (proba <= 0)) {
    stop("VecteurAleatoire, il est impossible d'avoir une proba <= à 0 : ",vproba);
  }

  nbnonnulles = 0;
  res = integer(n);
  
  while (nbnonnulles == 0) {
    vprob = runif(n, 0, 1);
    for (i in 1:n) {
      if (vprob[i] <= proba) {
        nbnonnulles <- nbnonnulles+1;
        res[i] <- 1;
      } else {
        res[i] <- 0;
      }
    }
  }

  res;
}




ensembleToDataFrameApprentissage <- function(Ens, Clas) {
  ##! @brief creation d'un data.frame à partir des classe et de
  ##! l'ensemble d'apprentissage
  ##! @param Ens : Ensemble
  ##! @param Clas : les classes
  ##!
  ##! La valeur retournée contient les classes (au début) et les variables
  ##! sous le nom Y1 Y2 ... ensuite
  
  nbindiv <- nrow(Ens);
  dim <- ncol(Ens);

  # if (length(Clas) != nbindiv) {
    # stop("Problème, ne nombre de ligne du tableau de classe ne correspond pas au nombre de ligne de l'ensemble : "+ nbindiv+ "!="+nrow(Clas));
  # }

  X <- matrix(nrow=nbindiv,ncol=(dim+1),0);
  noms <- character(dim+1);
 
 #variables names
  noms[1] <- "classe";
  for (i in 1:dim) {
    noms[i+1] <- paste("Y",i,sep="");
  }


  res <- data.frame(Clas,Ens);
  names(res) <- noms;

  res;
}



ensembleToDataFrameTest <- function(Ens) {
  ##! @brief creation d'un data.frame à partir des classe et de                  
  ##! l'ensemble d'apprentissage
  ##! @param Ens : Ensemble
  ##!
  ##! La valeur retournée contient les classes (au début) et les variables
  ##! sous le nom Y1 Y2 ... ensuite
  
 
  nbindiv <- nrow(Ens);
  dim <- ncol(Ens);


  X <- matrix(nrow=nbindiv,ncol=(dim),0);
  noms <- character(dim);
 
 # variables names  
  for (i in 1:dim) {
    noms[i] <- paste("Y",i,sep="");
  }

  res <- data.frame(Ens);
  names(res) <- noms;

  res;
}

####Fabien's way 
# dataFrameToFormula <- function(Df, nom_classe) {
  # ##! @brief génère une formule qui demande l'explication de la classe à
  # ##! partir d'un data.frame
  # ##! @param df : le data.frame
  # ##! @param nom_classe : le nom de la classe à expliquer
  # ##!
  # # if (!is.data.frame(Df)) {
    # # stop("Le paramettre DF n'est pas une data.frame");
  # # }
  # # if (!is.character(nom_classe)) {
    # # stop("Le nom de la classe n'est pas de type charactere");
  # # }

  # noms <- names(Df);
  # res <- paste(nom_classe, "~", sep=" ");
  # pos_clas <- grep(nom_classe, noms);

  # # if (length(pos_clas) != 1) {
    # # stop(paste("Je ne trouve pas ",nom_classe," dans la liste des noms de colonnes"));
  # # }

  # #t_nom <- character(length(noms)-1);
  # j <- 1;
  # premier <- TRUE;
  # for (i in 1:length(noms)) {
    # if (i != pos_clas) {
      # if (premier) {
        # res <- paste(res, noms[i], sep=" ");
        # premier <- FALSE;
      # } else {
        # res <- paste(res, noms[i], sep=" + ");
      # }
      # j <- j+1;
    # }
  # }

  # res;
# }


### my way :)
dataFrameFormula<-function(Df){
	noms<-names(Df);
	left<-head(noms,1);
	right<-paste(tail(noms,length(noms)-1),collapse=" +");
	result<-paste(c(left,right),collapse="~");
	result;
	
}




PropErreur = function(prevision, solution) {
  nbl = length(prevision);
  
  if (!(nbl == length(solution))) {
    stop("probleme avec les tailles ", nbl," n'est pas égale à ", length(solution));
  }
  
  res = 0;
  for (i in 1:nbl) {
    if ((prevision[i]!=solution[i])) {
      res = res + 1;
    }
  }
  res*1.0/nbl;
}






