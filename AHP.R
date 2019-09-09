library(ahp)
library(Matrix)
library(matrixcalc)


weight <- function(n){
  #'funkcja generuj¹ca wektor wag o zadanej dlugosci
  w <- runif(n,0,1)
  return(w/sum(w))
} 


weight_to_matrix <- function(v){
  #'funkcja generujaca macierz por. parami dla zadanego wektowa wag (macierz konsystentna)
  #'v - weigth vector
  if(sum(v)==1){
    A <- matrix(NA, length(v),length(v)) #macierz porównañ parami
    for(i in 1:length(v)){
      for(j in 1:length(v)){
        A[i,j] <- v[i]/v[j]
      }
    }
    return(A)}
  else (return("Podany wektor nie sumuje siê do 1!"))
}


pair_comp_matrix <- function(n){
  #'funkcja generuj¹ca losow¹ macierz porownan parami 
  #'n-wymiar macierzy
  A <- matrix(sample(c(1,2,3,4,5,6,7,8,9,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9), n*n,replace=TRUE),
              nrow=n, ncol=n)
  A[lower.tri(A)] <- 0
  for(j in 1:n-1){
    for(i in 2:n){
      if (i > j) A[i,j]=1/A[j,i]
    }
  }
  diag(A) = 1
  return(A)
}

CI_gen <- function(n){
  #'Symulacje RI
  CI_random1 <- c()
  for(i in 1:1000){
    macierz <- pair_comp_matrix(n)
    eigenv <- eigen(macierz)$values
    eigenv_max <- max(Re(eigenv[which(Im(eigenv) == 0)]))
    CI_random1[i] <- (eigenv_max - n)/(n-1)
  }
  return(mean(CI_random1))
}

set.seed(0)
CI_random <- c()
for(i in 3:10){
  CI_random <- append(CI_random, CI_gen(i))
}


is_consistent <- function(A){ 
  #'funkcja badaj¹ca, czy macierz jest spójna
  #'zwraca inf. TRUE dla m. spojnej, dla niespojnej zwraca lambda_max,index spójnoœci
  #'A - macierz porównañ parami
  if(dim(A)[1]!=dim(A)[2]) return("Macierz nie jest kwadratowa!")
  else{
    eig <- round(cbind(Re(eigen(A)$values),Im(eigen(A)$values)),10)
  }
  if(sum(eig[,2]==0)==dim(A)[1] & eig[1,1]==dim(A)[1] & sum(eig[-1,1]==0)==dim(A)[1]-1) return("TRUE")
  else{
    max_eig <- max(Re(eigen(A)$values[which(abs(Im(eigen(A)$values))< 1e-6)]))
    CI <- (max_eig - dim(A)[1])/(dim(A)[1] - 1)
    #CR <- CI/CI_random[dim(A[1])-2]
    return(c(max_eig,CI))
  }
}

#Hotele- przyk³ad
#Ateny, Rzym, Barcelona, Lizbona
#œrednia cena noclegu i wy¿ywienia, atrakcje turystyczne, klimat i pogoda
#Cena npclegu podana w Euro, dotyczy standardowego pokoju dwuosobowego, za³: ceny rosn¹ jednostajnie
#do iloœci osób w pokoju


#Œrednia cena noclegu i wy¿ywienia, dane odczytane ze strony https://www.priceoftravel.com/2427/europe-3-star-traveler-index/#35_8211_Rome_Italy
#odczytano dnia 18.03.2019 (dane za miasi¹c marzec)
#Pogoda i klimat odczytane z https://www.priceoftravel.com/14/italy/rome-prices
#dnia 18.03.2019
klimat <- matrix(c(33, 21,mean(c(33,21)), 0.5,28, 18,mean(c(28,18)), 2.0,28, 18,mean(c(28,18)), 0.5, 28, 18,mean(c(28,18)), 1.5),nrow=4, ncol=4)
colnames(klimat) <- c('Ateny', 'Barcelona', 'Lizbona', 'Rzym')
rownames(klimat) <- c('High st. C', 'Low st. C','Œrednia z High i Low', 'Opady cm')
nocleg <- matrix(c(80, 101, 82, 86),nrow=1,ncol=4, byrow=TRUE)
colnames(nocleg) <- c('Ateny', 'Barcelona', 'Lizbona', 'Rzym')
rownames(nocleg) <- c('Ceny noclegu EUR')

#Atrakcje turystyczne dla dzieci
#Rzym : http://loty-do-rzymu.pl/10-najlepszych-atrakcji-dla-dzieci-w-rzymie/
#Barcelona : https://www.dzieckowpodrozy.pl/barcelona-z-dzieckiem-atrakcje-co-zobaczyc-weekend/
#Lizbona : http://infolizbona.pl/lizbona-dla-dzieci-i-rodzin-najwazniejsze-atrakcje-dla-dzieci-w-lizbonie-zwiedzania-zdjecia/
#Ateny : https://www.podakropolem.pl/z-dziecmi-w-atenach/

#Macierze porównañ parami
r_kr1 <- matrix(c(1,5,9,3,1/5,1,7,1/4,1/9,1/7,1,1/5,1/3,4,5,1),nrow=4,ncol=4) #kryterium 1
r_kr2 <- matrix(c(1,1/6,1/4,1/2,6,1,4,4,4,1/4,1,5,2,1/4,1/5,1),nrow=4, ncol=4) #kryterium 2
r_kr3 <- matrix(c(1,5,3,7,1/5,1,1/2,3,1/3,2,1,5,1/7,1/3,1/5,1),nrow=4,ncol=4) #kryterium 3
r_kr_kr <- matrix(c(1,1/2,1/4,2,1,1/7,4,7,1),nrow=3,ncol=3) #kryteria miêdzy soba

eigen_r <- matrix(c(Re(eigen(r_kr1)$values[which(Im(eigen(r_kr1)$values)==0)]), max(Re(eigen(r_kr1)$values[which(Im(eigen(r_kr1)$values)==0)])),
             Re(eigen(r_kr2)$values[which(Im(eigen(r_kr2)$values)==0)]), max(Re(eigen(r_kr2)$values[which(Im(eigen(r_kr2)$values)==0)])),
             Re(eigen(r_kr3)$values[which(Im(eigen(r_kr3)$values)==0)]), max(Re(eigen(r_kr3)$values[which(Im(eigen(r_kr3)$values)==0)])),
             Re(eigen(r_kr_kr)$values[which(Im(eigen(r_kr_kr)$values)==0)]), NA, max(Re(eigen(r_kr_kr)$values[which(Im(eigen(r_kr_kr)$values)==0)]))),
             nrow=4 ,ncol=3 , byrow=TRUE) #macierz z rzeczywistymi wartosciami wlasnymi
colnames(eigen_r) <- c('Wartoœæ w³asna 1', 'Wartoœæ w³asna 2', 'Max wartoœæ w³asna')
rownames(eigen_r) <- c('Kryterium 1', 'Kryterium 2', 'Kryterium 3', 'Kryteria miêdzy sob¹')

#CI i CR dla przyk³adu z pracy
CI <- c()
CI[1] <- is_consistent(r_kr1)[2]
CI[2] <- is_consistent(r_kr2)[2]
CI[3] <- is_consistent(r_kr3)[2]
CI[4] <- is_consistent(r_kr_kr)[2]

CR <- c()
CR[1] <- CI[1] / 0.88
CR[2] <- CI[2] / 0.88
CR[3] <- CI[3] / 0.88
CR[4] <- CI[4] / 0.52
CR <- round(CR,3)


CI_gen <- function(n){
  #'Symulacje RI
  CI_random1 <- c()
  for(i in 1:1000){
    macierz <- pair_comp_matrix(n)
    eigenv <- eigen(macierz)$values
    eigenv_max <- max(Re(eigenv[which(Im(eigenv) == 0)]))
    CI_random1[i] <- (eigenv_max - n)/(n-1)
  }
  return(mean(CI_random1))
}

set.seed(0)
CI_random <- c()
for(i in 3:10){
  CI_random <- append(CI_random, CI_gen(i))
}
CI_random = round(CI_random,3)
#Szukanie najbli¿szej spojnej macierzy
#odzworowanie L i E
L <- function(A){
  return(log(A))
}

E <- function(A){
  return(exp(A))
}

ones <- function(n){
  #'indykator o n elementach
  return(rep(1,times=n))
}

U <- function(n){
  #'macierz U- macierz z³o¿na z samych jedynek
  return(matrix(rep(1,n*n), nrow=n, ncol=n))
}

projection <- function(A){
  #' rzut ortogonalny
  n <- dim(A)[1]
  return(1/n*(A%*%U(n) - t(A%*%U(n))))
}

closest_consistent <- function(A){
  #' najbli¿sza macierz spójna
  return(E(projection(L(A))))
}

czy_macierz_por_par <- function(A){
  #' test czy macierz jest macierz¹ porównañ parami
  licznik = 0
  for(i in c(1:dim(A)[1])){
    for(j in c(1:dim(A)[1])){
      if(A[i,j] != 1/A[j,i]) {licznik = licznik+1}
    }
  }
  if(licznik == 0){
    return('Tak')
  }
  else{
    return('Nie')
  }
}

#Przyk³ad z in¿ynierii oprogramowania

macierz_kryteria <- matrix(c(1, 1/3, 2, 1/2, 1/2, 1/7, 1/8, 1/2,
                             3,1,1/2,5,4,2,1,7,
                             1/2,2,1,5,2,2,1,5,
                             2,1/5,1/5,1,3,1/4,1/6,2,
                             2,1/4,1/2,1/3,1,1/4,1/7,2,
                             7,1/2,1/2,4,4,1,1/4,4,
                             8,1,1,6,7,4,1,7,
                             2,1/7,1/5,1/2,1/2,1/4,1/7,1), 
                           nrow = 8, ncol = 8, byrow = 'True')
max_eigen_kryteria <- round(is_consistent(macierz_kryteria)[1],3)
CR_macierz_kryteria <- round(is_consistent(macierz_kryteria)[2]/CI_random[6],3)
macierz_kryteria_spojna <- closest_consistent(macierz_kryteria)
wagi_kryteria <- round(Re(eigen(macierz_kryteria)$vector[1:8]/sum(eigen(macierz_kryteria)$vectors[1:8])),3) 
wagi_kryteria_c <- round(Re(eigen(macierz_kryteria_spojna)$vector[1:8]/sum(eigen(macierz_kryteria_spojna)$vectors[1:8])),3) 

macierz_K1 <- matrix(c(1, 3, 3, 2,5,1,
                       1/3 , 1 , 1/2 ,4 , 7 , 1/4,
                       1/3 , 2 , 1 , 4 , 5 , 1/4,
                       1/2 , 1/4 , 1/4 , 1 , 3 , 1/3,
                       1/5 , 1/7 , 1/5 ,1/3 , 1 , 1/7,
                       1 , 4 , 4 , 3 , 7 ,1), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_1 <- round(is_consistent(macierz_K1)[1],3)
CR_macierz_K1 <- round(is_consistent(macierz_K1)[2]/CI_random[4],3)
macierz_K1_spojna <- round(closest_consistent(macierz_K1),3)
wagi_K1 <- round(Re(eigen(macierz_K1)$vector[1:6]/sum(eigen(macierz_K1)$vectors[1:6])),3) 
wagi_K1_c <- round(Re(eigen(macierz_K1_spojna)$vector[1:6]/sum(eigen(macierz_K1_spojna)$vectors[1:6])),3) 



macierz_K2 <- matrix(c(1, 1/4, 1/9, 1/2,1/2,1/9,
                       4,1,1/2,3,5,1/3,
                       9,2,1,7,7,1/2,
                       2,1/3,1/7,1,1,1/4,
                       2,1/5,1/7,1,1,1/4,
                       9,3,2,1,4,1), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_2 <- round(is_consistent(macierz_K2)[1],3)
CR_macierz_K2 <- round(is_consistent(macierz_K2)[2]/CI_random[4],3)
macierz_K2_spojna <- closest_consistent(macierz_K2)
wagi_K2 <- round(Re(eigen(macierz_K2)$vector[1:6]/sum(eigen(macierz_K2)$vectors[1:6])),3) 
wagi_K2_c <- round(Re(eigen(macierz_K2_spojna)$vector[1:6]/sum(eigen(macierz_K2_spojna)$vectors[1:6])),3) 

macierz_K3 <- matrix(c(1, 1/7, 1/9, 1/3,1/4,1/9,
                       7,1,1/5,4,4,1/5,
                       9,5,1,8,6,1/4,
                       3,1/4,1/8,1,2,1/5,
                       4,1/4,1/6,1/2,1,1/4,
                       9,5,4,5,4,1), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_3 <- round(is_consistent(macierz_K3)[1],3)
CR_macierz_K3 <- round(is_consistent(macierz_K3)[2]/CI_random[4],3)
macierz_K3_spojna <- round(closest_consistent(macierz_K3),3)
wagi_K3 <- round(Re(eigen(macierz_K3)$vector[1:6]/sum(eigen(macierz_K3)$vectors[1:6])),3) 
wagi_K3_c <- round(Re(eigen(macierz_K3_spojna)$vector[1:6]/sum(eigen(macierz_K3_spojna)$vectors[1:6])),3) 

macierz_K4 <- matrix(c(1, 8, 3 ,3,2,2,
                       1/8, 1 , 1 ,1/2 , 1/2 , 1/5,
                       1/3 , 1 ,1 , 1/3 , 1/2 , 1/4,
                       1/3 ,2 , 3 , 1 , 1 , 1/3,
                       1/2 , 2 , 2 , 1 , 1 ,1/3,
                       1/2,5,4,3,3,1), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_4 <- round(is_consistent(macierz_K4)[1],3)
CR_macierz_K4 <- round(is_consistent(macierz_K4)[2]/CI_random[4],3)
macierz_K4_spojna <- round(closest_consistent(macierz_K4),3)
wagi_K4 <- round(Re(eigen(macierz_K4)$vector[1:6]/sum(eigen(macierz_K4)$vectors[1:6])),3) 
wagi_K4_c <- round(Re(eigen(macierz_K4_spojna)$vector[1:6]/sum(eigen(macierz_K4_spojna)$vectors[1:6])),3) 

macierz_K5 <- matrix(c(1, 2, 1/2 , 1/2 , 1/4 ,1/5,
                       1/2 , 1 , 1/5 , 3, 2 , 1/7,
                       2 , 5 ,1 , 2 , 1 , 1/6,
                       2 , 1/3 , 1/2 , 1 , 1/2 , 1/7,
                       4 , 1/2 , 1 , 2 , 1 ,1/2,
                       5 , 7 , 6 , 7 , 2 , 1 ), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_5 <- round(is_consistent(macierz_K5)[1],3)
CR_macierz_K5 <- round(is_consistent(macierz_K5)[2]/CI_random[4],3)
macierz_K5_spojna <- round(closest_consistent(macierz_K5),3)
wagi_K5 <- round(Re(eigen(macierz_K5)$vector[1:6]/sum(eigen(macierz_K5)$vectors[1:6])),3) 
wagi_K5_c <- round(Re(eigen(macierz_K5_spojna)$vector[1:6]/sum(eigen(macierz_K5_spojna)$vectors[1:6])),3) 

macierz_K6 <- matrix(c(1, 3, 1 , 1/3 , 1/2 ,4,
                       1/3 , 1 , 1/3 , 1/2 , 2 , 3,
                       1 , 3 ,1 , 3 , 2 , 4,
                       3 , 2 , 1/3 , 1 , 1/3 , 2,
                       2 , 1/2 , 1/2 , 3 , 1 ,5,
                       1/4 , 3 , 1/4 , 1/2 , 1/5 , 1 ), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_6 <- round(is_consistent(macierz_K6)[1],3)
CR_macierz_K6 <- round(is_consistent(macierz_K6)[2]/CI_random[4],3)
macierz_K6_spojna <- round(closest_consistent(macierz_K6),3)
wagi_K6 <- round(Re(eigen(macierz_K6)$vector[1:6]/sum(eigen(macierz_K6)$vectors[1:6])),3) 
wagi_K6_c <- round(Re(eigen(macierz_K6_spojna)$vector[1:6]/sum(eigen(macierz_K6_spojna)$vectors[1:6])),3) 

macierz_K7 <- matrix(c(1, 1/4, 1/7 , 1/2 , 1/2 ,1/9,
                       4 , 1 , 1/3 , 3 , 4 , 1/4,
                       7 , 3 ,1 , 5 , 4 , 1/2,
                       2 , 1/3 , 1/5 , 1 , 1/3 , 1/7,
                       2 , 1/4 , 1/4 , 3 , 1 ,1/5,
                       9 , 4 , 2 , 7 , 5 , 1 ), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_7 <- round(is_consistent(macierz_K7)[1],3)
CR_macierz_K7 <- round(is_consistent(macierz_K7)[2]/CI_random[4],3)
macierz_K7_spojna <- closest_consistent(macierz_K7)
wagi_K7 <- round(Re(eigen(macierz_K7)$vector[1:6]/sum(eigen(macierz_K7)$vectors[1:6])),3) 
wagi_K7_c <- round(Re(eigen(macierz_K7_spojna)$vector[1:6]/sum(eigen(macierz_K7_spojna)$vectors[1:6])),3) 


macierz_K8 <- matrix(c(1, 1/2, 2 , 1/3 , 1/3 , 4 ,
                       2 , 1 , 1/2 , 1/3 , 2 , 5,
                       1/2 , 2 ,1 , 4 , 4 , 5,
                       3 , 3 , 1/4 , 1 , 3 , 4,
                       3 , 1/2 , 1/4 , 1/3 , 1 ,3,
                       1/4 , 1/5 , 1/5 , 1/4 , 1/3 , 1 ), 
                     nrow = 6, ncol = 6, byrow = 'True')
max_eigen_8 <- round(is_consistent(macierz_K8)[1],3)
CR_macierz_K8 <- round(is_consistent(macierz_K8)[2]/CI_random[4],3)
macierz_K8_spojna <- round(closest_consistent(macierz_K8),3)
wagi_K8 <- round(Re(eigen(macierz_K8)$vector[1:6]/sum(eigen(macierz_K8)$vectors[1:6])),3) 
wagi_K8_c <- round(Re(eigen(macierz_K8_spojna)$vector[1:6]/sum(eigen(macierz_K8_spojna)$vectors[1:6])),3) 


ostateczny_wektor <- wagi_kryteria_c[1] * wagi_K1_c + wagi_kryteria_c[2] * wagi_K2 + wagi_kryteria_c[3] * wagi_K3_c + 
                    wagi_kryteria_c[4] * wagi_K4 + wagi_kryteria_c[5] * wagi_K5_c + wagi_kryteria_c[6] * wagi_K6_c +
                    wagi_kryteria_c[7] * wagi_K7 + wagi_kryteria_c[8] * wagi_K8_c

ostateczny_wektor <- round(ostateczny_wektor,3)

ostateczny_wektor1 <- wagi_kryteria_c[1] * wagi_K1 + wagi_kryteria_c[2] * wagi_K2 + wagi_kryteria_c[3] * wagi_K3 + 
  wagi_kryteria_c[4] * wagi_K4 + wagi_kryteria_c[5] * wagi_K5 + wagi_kryteria_c[6] * wagi_K6 +
  wagi_kryteria_c[7] * wagi_K7 + wagi_kryteria_c[8] * wagi_K8

ostateczny_wektor1 <- round(ostateczny_wektor1,3)

