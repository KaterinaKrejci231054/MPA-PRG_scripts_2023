load ("V:/MPA-PRG/exercise_11/HMM1.RData")
load ("V:/MPA-PRG/exercise_11/HMM2.RData")

matice_A <- HMM1$A
matice_B <- HMM1$B
vektor <- c("G", "G", "C")
len_vect <- length(vector)
nova_matice <- matrix(, nrow = length(HMM1$N), ncol = length(vektor))

print(HMM1$A)
print(HMM1$pi)
print(nova_matice)

p_HH <- HMM1$A[1,1] 
p_HL <- HMM1$A[1,2]
p_LH <- HMM1$A[2,1] 
p_LL <- HMM1$A[2,2] 

H_A <- HMM1$B[1,1]
H_C <- HMM1$B[1,2]
H_G <- HMM1$B[1,3]
H_T <- HMM1$B[1,4]

L_A <- HMM1$B[2,1] 
L_C <- HMM1$B[2,2]
L_G <- HMM1$B[2,3]
L_T <- HMM1$B[2,4]

start_H <- HMM1$pi[1]
start_L <- HMM1$pi[2]

PH_G1 <- start_H + H_G
PL_G1 <- start_L + L_G

nova_matice[1,1] <- PH_G1
nova_matice[2,1] <- PL_G1
print(nova_matice)


PH_G2 <- H_G + max((PH_G1 + p_HH), (PL_G1 + p_LH))
PL_G2 <- L_G + max((PL_G1 + p_LL), (PH_G1 + p_HL))

nova_matice[1,2] <- PH_G2
nova_matice[2,2] <- PL_G2
print(nova_matice)

PH_C3 <- H_C + max((PH_G2 + p_HH), (PL_G2 + p_LH))
PL_C3 <- L_C + max((PL_G2 + p_LL), (PH_G2 + p_HL))

nova_matice[1,3] <- PH_C3
nova_matice[2,3] <- PL_C3
print(nova_matice)

max_col_1 <- max(nova_matice[1, ], na.rm = TRUE)

if (max_col_1 == nova_matice[1, 1]){
  print("H")
}
if(max_col_1 == nova_matice[1, 2]){
  print("L")}

max_col_2 <- max(nova_matice[2, ], na.rm = TRUE)

if (max_col_2 == nova_matice[2, 1]){
  print("H")
}
if(max_col_2 == nova_matice[2, 2]){
  print("L")}

print(nova_matice[2, 1])
print(nova_matice)

max_col_3 <- max(nova_matice[3, ], na.rm = TRUE)

if (max_col_3 == nova_matice[3, 1]){
  print("H")
}
if(max_col_3 == nova_matice[3, 2]){
  print("L")}


