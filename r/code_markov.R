data<-read.csv("C:\\Users\\pc\\OneDrive\\msc statistics notes\\2 sem\\sp\\proj\\final_data.csv")
data
attach(data)

summary(data)

#explorartory data analysis
library(ggplot2)
library(dplyr)
# Histogram and Density
ggplot(data%>% filter(wnd_speed != "0"), aes(x = wnd_speed)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  geom_density(aes(y = after_stat(density)), color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Wind Speed Distribution", x = "Wind Speed", y = "Frequency")

ggplot(data%>% filter(wnd_speed != "0"), aes(y = wnd_speed)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot of Wind Speed", y = "Wind Speed")

ggplot(data%>% filter(wnd_state != "Unclassified"), aes(x = wnd_state)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  labs(title = "Wind State Frequencies", x = "Wind State", y = "Count")

ggplot(data%>% filter(! transition %in% c("LU","MU","UL","UU","UM")), aes(x = transition)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  labs(title = "Transition Frequencies", x = "Transition", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data%>% filter(wnd_state != "Unclassified"), aes(x = wnd_state, y = wnd_speed, fill = wnd_state)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Wind Speed by Wind State", x = "Wind State", y = "Wind Speed")

#library(dplyr)
ggplot(data%>% filter(wnd_state != "Unclassified"), aes(x = wnd_speed, fill = wnd_state)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ wnd_state, scales = "free") +
  theme_minimal() +
  labs(title = "Wind Speed Distributions by State", x = "Wind Speed", y = "Density")


#State Frequencies and state probabilities
x<-table(wnd_state == "Strong")
x[2]
p1=round(x[2]/length(wnd_state),6)
p1
y<-table(wnd_state == "Moderate")
y[2]
p2=round(y[2]/length(wnd_state),6)
p2
z<-table(wnd_state == "Light")
z[2]
p3=round(z[2]/length(wnd_state),6)
p3
PPP<-table(wnd_state == "Unclassified")
PPP
ppo<-round(PPP[2]/length(wnd_state),6)
ppo
#intial probability vector
p<-c(p1,p2,p3)

#transition frequencies
i11<-table(transition== "SS")
i11
i12<-table(transition== "SM")
i12
i13<-table(transition== "SL")
i13
i21<-table(transition== "MS")
i21
i22<-table(transition== "MM")
i22
i23<-table(transition== "ML")
i23
i31<-table(transition== "LS")
i31
i32<-table(transition== "LM")
i32
i33<-table(transition== "LL")
i33

#transition probabilities
p11<-round(i11[2]/(i11[2]+i12[2]+i13[2]),5)
p11
p12<-round(i12[2]/(i11[2]+i12[2]+i13[2]),5)
p12
p13<-round(i13[2]/(i11[2]+i12[2]+i13[2]),5)
p13
p21<-round(i21[2]/(i21[2]+i22[2]+i23[2]),5)
p21
p22<-round(i22[2]/(i21[2]+i22[2]+i23[2]),5)
p22
p23<-round(i23[2]/(i21[2]+i22[2]+i23[2]),5)
p23
p31<-round(i31[2]/(i31[2]+i32[2]+i33[2]),5)
p31
p32<-round(i32[2]/(i31[2]+i32[2]+i33[2]),5)
p32
p33<-round(i33[2]/(i31[2]+i32[2]+i33[2]),5)
p33

#transition frequency matrix
tfm<-matrix(c(i11[2],i12[2],i13[2],i21[2],i22[2],i23[2],i31[2],i32[2],i33[2]),nrow=3,byrow=TRUE)
tfm

#transition probability matrix
tpm<-round(matrix(c(p11,p12,p13,p21,p22,p23,p31,p32,p33),nrow=3,byrow=TRUE),5)
tpm

#intial state probabilities
P_S<-round(sum(p1*p11,p2*p21,p3*p31),5)
P_S
P_M<-round(sum(p1*p12,p2*p22,p3*p32),5)
P_M
P_L<-round(sum(p1*p13,p2*p23,p3*p33),5)
P_L
#one sequence
#pmf strong
P_S1_Y0<-0
for(k in 1:3){
  for(j in 2:3){
    P_S1_Y0<-round(P_S1_Y0+p[k]*tpm[k,j],6)
  }
}
P_S1_Y0
P_S1_Y1<-0
for(k in 1:3){
  P_S1_Y1<-round(P_S1_Y1+p[k]*tpm[k,1],6)
}

P_S1_Y1
mu_S1<-0
for(k in 1:3){
  mu_S1<-round(mu_S1+p[k]*tpm[k,1],5)
}
mu_S1
v_s1<-round((mu_S1)^2*P_S1_Y0+(1-mu_S1)^2*P_S1_Y1,5)
v_s1
v_s1<-round((mu_S1)-(mu_S1)^2,5)
v_s1
mu3_s1<-round((-mu_S1)^3*P_S1_Y0+(1-mu_S1)^3*P_S1_Y1,5)
mu3_s1
mu4_s1<-round((mu_S1)^4*P_S1_Y0+(1-mu_S1)^4*P_S1_Y1,5)
mu4_s1
b1_s1<-mu3_s1^2/(v_s1)^3
b1_s1
b1_s2<-round(mu4_s1/(v_s1)^2,5)
b1_s2
cv_s1<-(sqrt(v_s1)/P_S1_Y1)*100
cv_s1
mt_s1<-cat(P_S1_Y0,"+ e^t", P_S1_Y1)
Pit_s1<-cat(P_S1_Y0,"+ e^it", P_S1_Y1)
Pt_s1<-cat(P_S1_Y0,"+ s", P_S1_Y1)



#one sequence
#pmf moderate
P_M1_Y0<-0
for(k in 1:3){
  for(j in c(1,3)){
    P_M1_Y0<-round(P_M1_Y0+p[k]*tpm[k,j],5)
  }
}
P_M1_Y0
P_M1_Y1<-0
for(k in 1:3){
  P_M1_Y1<-round(P_M1_Y1+p[k]*tpm[k,2],5)
}
P_M1_Y1
mu_M1<-0
for(k in 1:3){
  mu_M1<-round(mu_M1+p[k]*tpm[k,2],6)
}
mu_M1
v_M1<-round((mu_M1)^2*P_M1_Y0+(1-mu_M1)^2*P_M1_Y1,5)
v_M1
v_M1<-round((mu_M1)-(mu_M1)^2,5)
v_M1
mu3_M1<-round((-mu_M1)^3*P_M1_Y0+(1-mu_M1)^3*P_M1_Y1,5)
mu3_M1
mu4_M1<-round((mu_M1)^4*P_M1_Y0+(1-mu_M1)^4*P_M1_Y1,5)
mu4_M1
b1_M1<-mu3_M1^2/(v_M1)^3
b1_M1
b1_M2<-round(mu4_M1/(v_M1)^2,5)
b1_M2
cv_M1<-(sqrt(v_M1)/P_M1_Y1)*100
cv_M1
mt_M1<-cat(P_M1_Y0,"+ e^t", P_M1_Y1)
Pit_M1<-cat(P_M1_Y0,"+ e^it", P_M1_Y1)
Pt_s1<-cat(P_M1_Y0,"+ s", P_M1_Y1)



#one sequence
#pmf Light
P_L1_Y0<-0
for(k in 1:3){
  for(j in c(1,2)){
    P_L1_Y0<-round(P_L1_Y0+p[k]*tpm[k,j],4)
  }
}
P_L1_Y0
P_L1_Y1<-0
for(k in 1:3){
  P_L1_Y1<-round(P_L1_Y1+p[k]*tpm[k,3],4)
}
P_L1_Y1
mu_L1<-0
for(k in 1:3){
  mu_L1<-round(mu_L1+p[k]*tpm[k,3],4)
}
mu_L1
v_L1<-round((mu_L1)^2*P_L1_Y0+(1-mu_M1)^2*P_L1_Y1,4)
v_L1
v_L1<-round((mu_L1)-(mu_L1)^2,4)
v_L1
mu3_L1<-round((-mu_L1)^3*P_L1_Y0+(1-mu_L1)^3*P_L1_Y1,4)
mu3_L1
mu4_L1<-round((mu_L1)^4*P_L1_Y0+(1-mu_L1)^4*P_L1_Y1,4)
mu4_L1
b1_L1<-round(mu3_L1^2/(v_L1)^3,4)
b1_L1
b1_L2<-round(mu4_L1/(v_L1)^2,4)
b1_L2
cv_L1<-round((sqrt(v_L1)/P_L1_Y1)*100,4)
cv_L1
mt_L1<-cat(P_L1_Y0,"+ e^t", P_L1_Y1)
Pit_L1<-cat(P_L1_Y0,"+ e^it", P_L1_Y1)
Pt_L1<-cat(P_L1_Y0,"+ s", P_L1_Y1)





#2 seq
#STRONG
p2i<-0
for(k in 2:3){
  p2i<-round(p2i+tpm[2,k],4)
}
p2i
p3i <- round(sum(tpm[3, 2:3]), 4)
print(p3i)
p1i <- round(sum(tpm[1, 2:3]), 4)
print(p1i)
as_11<-round(P_M*p2i+P_L*p3i,4)
as_11
as_12<-round(P_S*p1i+P_M*p21+P_L*p31,4)
as_12
as_13<-round(p11*P_S,4)
as_13
round(as_11+as_12+as_13,1)
mu2_s<-round(as_12+2*as_13,1)

mu2_s
v2_s<-round((mu2_s)^2*as_11+(1-mu2_s)^2*as_12+(2-mu2_s)^2*as_13,4)
v2_s
mu23_s<-round((-mu2_s)^3*as_11+(1-mu2_s)^3*as_12+(2-mu2_s)^3*as_13,4)
mu23_s

be12_s<-round(mu23_s^2/(mu2_s)^3,4)
be12_s
be22_s<-round(((mu2_s)^4*as_11+(1-mu2_s)^4*as_12+(2-mu2_s)^4*as_13)^2/mu23_s^2,4)
be22_s
cv2_s<-round(sqrt(v2_s)/mu2_s*100,4)
cv2_s
mt_s2<-cat(as_11,"+ e^t", as_12, "+ e^t", as_13)
Pit_s2<-cat(as_11,"+ e^it", as_12, "+ ei^t", as_13)
Pt_s2<-cat(as_11,"+ s", as_12, "+ s", as_13)

#2 seq
#moderate
p2i_M<-0
for(k in c(1,3)){
  p2i_M<-round(p2i_M+tpm[2,k],4)
}
p2i_M
p3i_M <- round(sum(tpm[3, c(1,3)]), 4)
print(p3i)
p1i_M <- round(sum(tpm[1, c(1,3)]), 4)
print(p1i_M)
aM_11<-round(P_S*p2i_M+P_L*p3i_M,4)
aM_11
aM_12<-round(P_M*p2i_M+P_S*p12+P_L*p32,4)
aM_12
aM_13<-round(p22*P_M,4)
aM_13
round(aM_11+aM_12+aM_13,1)
mu2_M<-round(aM_12+2*aM_13,4)
mu2_M
v2_M<-round((mu2_M)^2*aM_11+(1-mu2_M)^2*aM_12+(2-mu2_M)^2*aM_13,4)
v2_M
mu23_M<-round((-mu2_M)^3*aM_11+(1-mu2_M)^3*aM_12+(2-mu2_s)^3*aM_13,4)
mu23_M
be12_M<round(-mu23_M^2/(mu2_M)^3,4)
be12_M
be22_M<-round(((mu2_M)^4*aM_11+(1-mu2_M)^4*aM_12+(2-mu2_M)^4*aM_13)/mu23_M^2,4)
be22_M
cv2_M<-round(sqrt(v2_M)/mu2_M*100,4)
cv2_M
mt_M2<-cat(aM_11,"+ e^t", aM_12, "+ e^t", aM_13)
Pit_M2<-cat(aM_11,"+ e^it", aM_12, "+ ei^t", aM_13)
Pt_M2<-cat(aM_11,"+ s", aM_12, "+ s", aM_13)


#2 seq
#light
p1i_L<-0
for(k in c(1,2)){
  p1i_L<-round(p1i_L+tpm[1,k],4)
}
p1i_L

p2i_L <- round(sum(tpm[2, c(1,2)]), 4)
print(p2i_L)
p3i_L <- round(sum(tpm[3, c(1,2)]), 4)
print(p3i_L)
aL_11<-round(P_S*p1i_L+P_M*p2i_L,4)
aL_11
aL_12<-round(P_L*p3i_L+P_S*p13+P_M*p23,4)
aL_12
aL_13<-round(p33*P_L,4)
aL_13
round(aL_11+aL_12+aL_13,1)
mu2_L<-round(aL_12+2*aL_13,4)
mu2_L
v2_L<-round((mu2_L)^2*aL_11+(1-mu2_L)^2*aL_12+(2-mu2_L)^2*aL_13,4)
v2_L
mu23_L<-round((-mu2_L)^3*aL_11+(1-mu2_L)^3*aL_12+(2-mu2_L)^3*aL_13,4)
mu23_L
be12_L<-round(mu23_L^2/(mu2_L)^3,4)
be12_L
be22_L<-round(((mu2_L)^4*aL_11+(1-mu2_L)^4*aL_12+(2-mu2_L)^4*aL_13)/mu23_L^2,4)
be22_L
cv2_L<-round(sqrt(v2_L)/mu2_L*100,4)
cv2_L
mt_L2<-cat(aL_11,"+ e^t", aL_12, "+ e^t", aL_13)
Pit_L2<-cat(aL_11,"+ e^it", aL_12, "+ ei^t", aL_13)
Pt_L2<-cat(aL_11,"+ s", aL_12, "+ s", aL_13)









#3 seq
#STRONG
p2i_S <- round(sum(tpm[2, 1:2]), 6)
print(p2i_S)
p3i_S <- round(sum(tpm[3 , 2:3]), 6)
print(p3i_S)
pi1_S <- round(sum(tpm[1:2, 1]), 6)
print(pi1_S)
pi2_S <- round(sum(tpm[1:2, 1]), 6)
print(pi2_S)
pi31_S <- round(sum(tpm[c(1,3), 1]), 6)
print(pi31_S)
pi11_S <- round(sum(tpm[c(1,3), 1]), 6)

print(pi11_S)
b11_S<-round(P_M*(p22*p2i_S+p23*p3i_S)+P_L*(p32*p2i_S+p33*p3i_S),6)
b11_S
b12_S<-round(P_M*(p21*pi2_S+p21*p13+p23*p31)+P_L*(p31*pi31_S+p32*p21+p12*p31)+P_S*(p21*p2i_S+p13*p3i_S),6)
b12_S
b13_S<-round(P_S*(p12*pi1_S+p13*pi11_S)+p11*(p21*P_M+p31*P_L),6)
b13_S

b14_S<-round(p11^2*P_S,6)
b14_S
round(b13_s+b11_s+b12_s+b14_s,1)
mu3_S<-round(b12_S+2*b13_S+3*b14_S,6)
mu3_S
v3_S<-round((mu3_S)^2*b11_S+(1-mu3_S)^2*b12_S+(2-mu3_S)^2*b12_S+(3-mu3_S)^2*b14_S,6)
v3_S
mu33_S<-round((-mu3_S)^2*b11_S+(1-mu3_S)^2*b12_S+(2-mu3_S)^2*b12_S+(3-mu3_S)^2*b14_S,6)
mu33_S
be13_S<-round(mu33_S^2/(mu3_S)^3,6)
be13_S
be32_S<-round(((mu3_S)^4*b11_S+(1-mu3_S)^4*b12_S+(2-mu3_S)^4*b12_S+(3-mu3_S)^4*b14_S)/mu33_S^2,6)
be32_s
cv3_S<-round(sqrt(v3_S)/mu3_S*100,6)
cv3_S
mt_S3<-cat(b11_S,"+ e^t(", b12_S, "+ e^t", b13_S, "+ e^2t", b14_S, ")")
Pit_s3<-cat(b11_s,"+ e^it(", b12_s, "+ e^it", b13_s, "+ e^2it", b14_s, ")")
Pt_s3<-cat(b11_s,"+ s(", b12_s, "+ s", b13_s, "+ s^2", b14_s, ")")


#3 seq
p1i_M <- round(sum(tpm[1, c(1,3)]), 6)
print(p1i_M)
p2i_M<- round(sum(tpm[2, c(1,3)]), 6)
print(p2i_M)
p3i_M<- round(sum(tpm[3, c(1,3)]), 6)
print(p3i_M)
pi1_M <- round(sum(tpm[1:2, 1]), 6)
print(pi1_M)
pi2_M <- round(sum(tpm[1:2, 2]), 6)
print(pi2_M)

b11_M<-round(P_S*(p11*p1i_M+p13*p3i_M)+P_L*(p31*p1i_M+p33*p3i_M),6)
b11_M
b12_M<-round(P_M*(p21*p1i_M+p23*p3i_M)+P_S*(p12*pi1_M+p32*p13+p12*p23)+P_L*(p32*p2i_M+p31*p12+p33*p32),6)
b12_M
b13_M<-round(P_M*(p21*pi2_M+p23*pi2_M)+p22*(p12*P_S+p32*P_L),6)
b13_M
b14_M<-round(p22^2*P_M,6)
b14_M
round(b13_M+b11_M+b12_M+b14_M,1)
mu3_M<-round(b12_M+2*b13_M+3*b14_M,4)
mu3_M
v3_M<-round((mu3_M)^2*b11_M+(1-mu3_M)^2*b12_M+(2-mu3_M)^2*b12_M+(3-mu3_M)^2*b14_M,6)
v3_M
mu33_M<-round((-mu3_M)^2*b11_M+(1-mu3_M)^2*b12_M+(2-mu3_M)^2*b12_M+(3-mu3_M)^2*b14_M,6)
mu33_M
be13_M<-round(mu33_M^2/(mu3_M)^3,6)
be13_M
be32_M<-round(((mu3_M)^4*b11_M+(1-mu3_M)^4*b12_M+(2-mu3_M)^4*b12_M+(3-mu3_M)^4*b14_M)^2/mu33_M^2,6)
be32_M
cv3_M<-sqrt(v3_M)/mu3_M*100
cv3_M
mt_M1<-cat(b11_M,"+ e^t(", b12_M, "+ e^t", b13_M, "+ e^2t", b14_M, ")")
Pit_M1<-cat(b11_M,"+ e^it(", b12_M, "+ e^it", b13_M, "+ e^2it", b14_M, ")")
Pt_M1<-cat(b11_M,"+ s(", b12_M, "+ s", b13_M, "+ s^2", b14_M, ")")






#3 seq
p1i_L <- round(sum(tpm[1, c(1,2)]), 6)
print(p1i_L)
p2i_L<- round(sum(tpm[2, c(1,2)]), 6)
print(p2i_L)
p3i_L<- round(sum(tpm[3, c(1,2)]), 6)
print(p3i_L)
pi1_L <- round(sum(tpm[c(1,3), 1]), 6)
print(pi1_L)
pi3_L <- round(sum(tpm[c(1,3), 3]), 6)
print(pi3_L)
b11_L<-round(P_S*(p11*p1i_L+p12*p2i_L)+P_M*(p21*p1i_L+p22*p2i_L),6)

b11_L
b12_L<-round(P_L*(p31*p1i_L+p32*p2i_L)+P_S*(p13*pi1_L+p32*p13+p12*p23)+P_M*(p23*p3i_L+p31*p12+p22*p23),6)
b12_L
b13_L<-round(P_L*(p32*p3i_L+p31*pi3_M)+p33*(p13*P_S+p23*P_M),6)
b13_L
b14_L<-round(p33^2*P_L,6)
b14_L
round(b13_L+b11_L+b12_L+b14_L,1)
mu3_L<-round(b12_L+2*b13_L+3*b14_L,4)
mu3_L
v3_L<-round((mu3_L)^2*b11_L+(1-mu3_L)^2*b12_L+(2-mu3_L)^2*b12_L+(3-mu3_L)^2*b14_L,6)
v3_L
mu33_L<-round((-mu3_L)^2*b11_L+(1-mu3_L)^2*b12_L+(2-mu3_L)^2*b12_L+(3-mu3_L)^2*b14_L,6)
mu33_L
be13_L<-round(mu33_L^2/(mu3_L)^3,6)
be13_L
be32_L<-round(((mu3_L)^4*b11_L+(1-mu3_L)^4*b12_L+(2-mu3_L)^4*b12_L+(3-mu3_L)^4*b14_L)/mu33_L^2,6)
be32_L
cv3_L<-sqrt(v3_L)/mu3_L*100
cv3_L
mt_L1<-cat(b11_L,"+ e^t(", b12_L, "+ e^t", b13_L, "+ e^2t", b14_L, ")")
Pit_L1<-cat(b11_L,"+ e^it(", b12_L, "+ e^it", b13_L, "+ e^2it", b14_L, ")")
Pt_L1<-cat(b11_L,"+ s(", b12_L, "+ s", b13_L, "+ s^2", b14_L, ")")


#predicitions
MSL<-mean(data$wnd_speed[data$wnd_state=="Light"])
MSL
MSM<-mean(data$wnd_speed[data$wnd_state=="Moderate"])
MSM
MSS<-mean(data$wnd_speed[data$wnd_state=="Strong"])
MSS
MS<-as.matrix(c(MSS,MSM,MSL),nrow=3)
MS


# Function to check convergence of matrix powers
get_stationary_tpm <- function(tpm, tol = 1e-6, max_iter = 1000) {
  prev <- tpm
  for (i in 2:max_iter) {
    current <- prev %*% tpm
    if (max(abs(current - prev)) < tol) {
      return(list(step = i, matrix = current))
    }
    prev <- current
  }
  return(list(step = max_iter, matrix = current))
}

# Run the function
result <- get_stationary_tpm(tpm)

# Print when convergence occurs
cat("Stationary TPM occurs at step:", result$step, "\n")
print(round(result$matrix, 4))

pn<-result$matrix
MS
ERR<-pn%*%MS
ERR




y_prev<-c(0,0,1.5)
y_prev
predictions <- matrix(0, nrow = 30, ncol = 1)
for(t in 1:30){
  y_p<-y_prev
  Y_S <-  ERR[1]* y_p[1] + y_p[1] # Predict next day's value
  Y_M <-  ERR[1]* y_p[2] + y_p[2]        # Store the prediction
  Y_l <-  ERR[1]* y_p[3] + y_p[3]
  psp<-Y_S*mu_S1+Y_M*mu_M1+Y_l*mu_L1
  predictions[t] <- psp
  if(psp<4.44){
    y_prev<-c(0,0,psp)
  }else if(4.44 <= psp && psp <= 8.33){
    y_prev<-c(0,psp,0)
  }else if(psp>8.33){
    y_prev<-c(psp,0,0)
  }
  
  #y_prev<-c(Y_S,Y_M,Y_l)
}
predictions
pre<-predictions
#write.csv(predictions,file="pre_data.csv")
states <- matrix(0, nrow = 30, ncol = 1)
for(t in 1:30){
  if(predictions[t]<4.44){
    states[t]<-"L"
  }else if(4.44 <= predictions[t] && predictions[t] <= 8.33){
    states[t]<-"M"
  }else if(predictions[t]>8.33){
    states[t]<-"S"
  }
}
states
pred_data<-data.frame(predictions,states)
pred_data

ds<-read.csv("C:\\Users\\pc\\OneDrive\\msc statistics notes\\2 sem\\proj\\actual.csv")
ds
actual<-data.frame(ds$wnd_speed[1:30],ds$State[1:30])
actual
#chi square
actual_freq <- table(actual$ds.State.1.30.)
predicted_freq <- table(pred_data$states)
actual_freq 
# Align and make sure both have same levels
actual_freq <- actual_freq[c("L", "M")]
predicted_freq <- predicted_freq[c("L", "M")]
predicted_freq
# Perform Chi-square test
chisq.test(x = actual_freq, p = predicted_freq / sum(predicted_freq))












# Initialize parameters
set.seed(123)  # For reproducibility
n_days <- 30
predictions <- numeric(n_days)
states <- character(n_days)
initial_state <- "Light"  # Starting state (can be adjusted based on y_prev)
current_state <- initial_state

# Mean wind speeds for each state (from your code)
MS <- c(MSS, MSM, MSL)  # Mean wind speeds for Strong, Moderate, Light
names(MS) <- c("Strong", "Moderate", "Light")

# Transition probability matrix (from your code)
tpm <- matrix(c(p11, p12, p13, p21, p22, p23, p31, p32, p33), nrow = 3, byrow = TRUE)
rownames(tpm) <- colnames(tpm) <- c("Strong", "Moderate", "Light")

# Prediction loop
for (t in 1:n_days) {
  # Sample the next state based on the current state's transition probabilities
  next_state <- sample(c("Strong", "Moderate", "Light"), size = 1, prob = tpm[current_state, ])
  
  # Assign the predicted state
  states[t] <- next_state
  
  # Assign the predicted wind speed as the mean wind speed for the predicted state
  predictions[t] <- MS[next_state]
  
  # Update the current state for the next iteration
  current_state <- next_state
}

# Create a data frame with predictions and states
pred_data <- data.frame(wnd_speed = predictions, state = states)
print(pred_data)











# Set seed for reproducibility
set.seed(123)

# Define parameters
n_days <- 30
predictions <- numeric(n_days)
states <- character(n_days)

# Transition probability matrix (from your code)
tpm <- matrix(c(p11, p12, p13, p21, p22, p23, p31, p32, p33), nrow = 3, byrow = TRUE)
rownames(tpm) <- colnames(tpm) <- c("Strong", "Moderate", "Light")

# Mean wind speeds (from your code)
MS <- c(MSS, MSM, MSL)
names(MS) <- c("Strong", "Moderate", "Light")

# Calculate standard deviations for each state to add variability
# Assuming 'data' from final_data.csv is available
sd_S <- sd(data$wnd_speed[data$wnd_state == "Strong"], na.rm = TRUE)
sd_M <- sd(data$wnd_speed[data$wnd_state == "Moderate"], na.rm = TRUE)
sd_L <- sd(data$wnd_speed[data$wnd_state == "Light"], na.rm = TRUE)
SD <- c(sd_S, sd_M, sd_L)
names(SD) <- c("Strong", "Moderate", "Light")

# Initial state and wind speed (based on y_prev = c(0, 0, 1.5), assuming Light)
initial_state <- "Light"
initial_wind_speed <- MS["Light"]  # Use MSL instead of 1.5 for consistency
if (abs(1.5 - MS["Light"]) > 0.1) {
  cat("Warning: Initial wind speed (1.5) differs from MSL (", MS["Light"], "). Using MSL.\n")
}

# Prediction loop with variability
for (t in 1:n_days) {
  if (t == 1) {
    states[t] <- "Light"
    predictions[t] <- rnorm(1, mean = MS["Light"], sd = SD["Light"])
  } else {
    current_state <- states[t - 1]
    next_state <- sample(c("Strong", "Moderate", "Light"), size = 1, prob = tpm[current_state, ])
    states[t] <- next_state
    predictions[t] <- rnorm(1, mean = MS[next_state], sd = SD[next_state])
  }
  # Ensure wind speeds are non-negative (wind speed can't be negative)
  if (predictions[t] < 0) predictions[t] <- 0
}

# Convert states to match actual.csv format (L, M, S)
states <- factor(states, levels = c("Light", "Moderate", "Strong"), labels = c("L", "M", "S"))

# Create prediction data frame
pred_data <- data.frame(wnd_speed = round(predictions,1), state = states)
print(head(pred_data))
pred_data
# Save predictions (optional)
# write.csv(pred_data, file = "pre_data.csv", row.names = FALSE)
# Load actual data
ds <- read.csv("C:\\Users\\pc\\OneDrive\\msc statistics notes\\2 sem\\proj\\actual.csv")
actual <- data.frame(wnd_speed = ds$wnd_speed[1:30], state = ds$State[1:30])

# Ensure actual states are in correct format
actual$state <- factor(actual$state, levels = c("L", "M", "S"))

# Align predicted states
pred_data$state <- factor(pred_data$state, levels = c("L", "M", "S"))

# Compute error metrics
# Mean Absolute Error (MAE) for wind speeds
mae <- mean(abs(actual$wnd_speed - pred_data$wnd_speed), na.rm = TRUE)
cat("Mean Absolute Error (MAE) for wind speeds:", round(mae, 2), "\n")

# Root Mean Squared Error (RMSE) for wind speeds
rmse <- sqrt(mean((actual$wnd_speed - pred_data$wnd_speed)^2, na.rm = TRUE))
cat("Root Mean Squared Error (RMSE) for wind speeds:", round(rmse, 2), "\n")

# State prediction accuracy
accuracy <- mean(actual$state == pred_data$state, na.rm = TRUE)
cat("State prediction accuracy:", round(accuracy * 100, 2), "%\n")

# Chi-square test for state frequencies
actual_freq <- table(actual$state)
predicted_freq <- table(pred_data$state)
all_states <- c("L", "M", "S")
actual_freq <- actual_freq[all_states]; actual_freq[is.na(actual_freq)] <- 0
predicted_freq <- predicted_freq[all_states]; predicted_freq[is.na(predicted_freq)] <- 0

nonzero_states <- which(predicted_freq > 0 & actual_freq > 0)
chisq_result <- chisq.test(x = actual_freq[nonzero_states],
                           p = predicted_freq[nonzero_states] / sum(predicted_freq[nonzero_states]))
print(chisq_result)
chisq_result <- chisq.test(x = actual_freq, p = predicted_freq / sum(predicted_freq))
print(chisq_result)

actual_freq
predicted_freq

ERR









# Step 1: Mean wind speed per state
MSS <- mean(data$wnd_speed[data$wnd_state == "Strong"])
MSM <- mean(data$wnd_speed[data$wnd_state == "Moderate"])
MSL <- mean(data$wnd_speed[data$wnd_state == "Light"])
MS <- matrix(c(MSS, MSM, MSL), ncol = 1)

# Step 2: Initialize state vector (100% in Light state)
y_prev <- matrix(c(0, 0, 1), nrow = 1)

# Step 3: Prepare empty vector to store predictions
predictions <- numeric(30)

# Step 4: Loop for each time step
for (t in 1:30) {
  y_next <- y_prev %*% tpm           # Predict next state probabilities
  pred <- y_next %*% MS              # Compute expected wind speed
  predictions[t] <- as.numeric(pred) # Store prediction
  y_prev <- y_next                   # Update for next time step
}

# View predictions
print(predictions)








set.seed(123)  # For reproducibility

states <- c("S", "M", "L")
tpm <- tpm  # Your 3x3 transition probability matrix
MS <- c(S = MSS, M = MSM, L = MSL)

# Step 1: Start from a known state
current_state <- "L"
predictions <- numeric(30)
state_sequence <- character(30)

for (t in 1:30) {
  # Store predicted wind speed
  predictions[t] <- MS[current_state]
  state_sequence[t] <- current_state
  
  # Choose next state based on transition probabilities
  next_state <- sample(states, size = 1, prob = tpm[which(states == current_state), ])
  current_state <- next_state
}

# Combine result
result <- data.frame(Day = 1:30, State = state_sequence, WindSpeed = predictions)
print(result)






table(data$wnd_state)
states <- c("Strong", "Moderate", "Light")
init_state = "Light"
predict_markov_sim_variable <- function(tpm, data, init_state = "Light", n_days = 30, seed = 123) {
  set.seed(seed)
  
  # Match the state names to your actual data
  states <- c("Strong", "Moderate", "Light")
  
  if (!(init_state %in% states)) {
    stop("Initial state not found in defined states.")
  }
  
  current_state <- init_state
  predictions <- numeric(n_days)
  state_seq <- character(n_days)
  
  for (t in 1:n_days) {
    # Sample from actual wind speeds in that state
    state_data <- data$wnd_speed[data$wnd_state == current_state]
    
    if (length(state_data) == 0) {
      stop(paste("No data found for state:", current_state))
    }
    
    sampled_speed <- sample(state_data, size = 1)
    predictions[t] <- sampled_speed
    state_seq[t] <- current_state
    
    # Move to next state
    current_index <- match(current_state, states)
    next_state <- sample(states, size = 1, prob = tpm[current_index, ])
    current_state <- next_state
  }
  
  return(data.frame(Day = 1:n_days, State = state_seq, WindSpeed = predictions))
}
predicted_df <- predict_markov_sim_variable(tpm = tpm, data = data, init_state = "Light", n_days = 30)
print(predicted_df)











# Assuming your data has a clean 'wnd_state' column with values like "Strong", "Moderate", "Light"
states <- data$wnd_state
n <- length(states)

# Create list of transitions
transitions <- data.frame(
  prev = states[1:(n - 2)],
  current = states[2:(n - 1)], nextt = states[3:n]
)

# Count frequencies of (prev, current) → next
library(dplyr)
trans_freq_2nd <- transitions %>%
  count(prev, current, nextt)

print(head(trans_freq_2nd))

trans_prob_2nd <- trans_freq_2nd %>%
  group_by(prev, current) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

trans_prob_2nd

simulate_2nd_order <- function(trans_prob_2nd, init_states = c("Light", "Light"), n_days = 30) {
  states <- unique(trans_prob_2nd$nextt)
  current <- init_states[2]
  prev <- init_states[1]
  
  simulated <- character(n_days)
  simulated[1:2] <- init_states
  
  for (t in 3:n_days) {
    # Filter matching transitions
    options <- trans_prob_2nd %>% 
      filter(prev == simulated[t - 2], current == simulated[t - 1])
    
    # Handle unseen pairs by fallback (e.g., repeat last state)
    if (nrow(options) == 0) {
      simulated[t] <- simulated[t - 1]
    } else {
      simulated[t] <- sample(options$nextt, size = 1, prob = options$prob)
    }
  }
  
  return(simulated)
}
simulated_states <- simulate_2nd_order(trans_prob_2nd, init_states = c("Light", "Light"), n_days = 30)
set.seed(123)
predicted_winds <- sapply(simulated_states, function(state) {
  sample(data$wnd_speed[data$wnd_state == state], size = 1)
})

predicted_df <- data.frame(Day = 1:30, State = simulated_states, WindSpeed = predicted_winds)
predicted_df





library(dplyr)

# Step 1: Clean data
data1 <- data %>% filter(wnd_state != "Unclassified")

# Step 2: Build 2nd-order transition probabilities
transition_probs_2nd <- data1 %>%
  group_by(transition, wnd_state) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(transition) %>%
  mutate(prob = n / sum(n))

# Step 3: Simulate future wind states
simulate_2nd_order_chain <- function(trans_prob_table, init_pair = "LL", n_days = 30) {
  predictions <- character(n_days)
  predictions[1] <- substr(init_pair, 1, 1)
  predictions[2] <- substr(init_pair, 2, 2)
  
  for (t in 3:n_days) {
    pair <- paste0(predictions[t - 2], predictions[t - 1])
    options <- trans_prob_table %>% filter(transition == pair)
    
    if (nrow(options) == 0) {
      predictions[t] <- predictions[t - 1]  # fallback
    } else {
      predictions[t] <- sample(options$wnd_state, size = 1, prob = options$prob)
    }
  }
  
  return(predictions)
}

# Step 4: Predict states and wind speeds
map_state <- function(letter) {
  if (letter == "L") return("Light")
  if (letter == "M") return("Moderate")
  if (letter == "S") return("Strong")
  return(NA)
}
sim_states <- simulate_2nd_order_chain(transition_probs_2nd, init_pair = "LL", n_days = 30)

sim_states <- sapply(sim_states, map_state)
# Safely sample wind speeds for each simulated state
sim_speeds <- sapply(sim_states, function(s) {
  eligible_speeds <- data1$wnd_speed[data1$wnd_state == s]
  
  # If state has no matching speeds (just in case), fallback to 0
  if (length(eligible_speeds) == 0) {
    return(NA)  # or 0 or mean(data1$wnd_speed)
  } else {
    return(sample(eligible_speeds, size = 1))
  }
})


predicted_df <- data.frame(Day = 1:30, State = sim_states, WindSpeed = sim_speeds)

# Done!
print(predicted_df)






# Step 1: Clean data
data1 <- data %>% filter(wnd_state != "Unclassified")

# Step 2: Create transition probability table using L, M, S
# First map wnd_state to one-letter code
data1 <- data1 %>%
  mutate(state_code = case_when(
    wnd_state == "Light" ~ "L",
    wnd_state == "Moderate" ~ "M",
    wnd_state == "Strong" ~ "S"
  ))

# Step 3: Build transition probabilities
transition_probs_2nd <- data1 %>%
  group_by(transition, state_code) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(transition) %>%
  mutate(prob = n / sum(n))

# Step 4: Simulate 2nd-order Markov chain
simulate_2nd_order_chain <- function(trans_prob_table, init_pair = "LL", n_days = 300) {
  predictions <- character(n_days)
  predictions[1] <- substr(init_pair, 1, 1)
  predictions[2] <- substr(init_pair, 2, 2)
  
  for (t in 3:n_days) {
    pair <- paste0(predictions[t - 2], predictions[t - 1])
    options <- trans_prob_table %>% filter(transition == pair)
    
    if (nrow(options) == 0) {
      predictions[t] <- predictions[t - 1]  # fallback
    } else {
      predictions[t] <- sample(options$state_code, size = 1, prob = options$prob)
    }
  }
  
  return(predictions)
}

# Step 5: Simulate and map to full state names
sim_codes <- simulate_2nd_order_chain(transition_probs_2nd, init_pair = "LL", n_days = 300)

map_state <- function(letter) {
  if (letter == "L") return("Light")
  if (letter == "M") return("Moderate")
  if (letter == "S") return("Strong")
  return(NA)
}

sim_states <- sapply(sim_codes, map_state)

# Step 6: Sample wind speeds
sim_speeds <- sapply(sim_states, function(s) {
  eligible_speeds <- data1$wnd_speed[data1$wnd_state == s]
  if (length(eligible_speeds) == 0) {
    return(NA)
  } else {
    return(sample(eligible_speeds, size = 1))
  }
})

# Step 7: Final predicted data frame
predicted_df <- data.frame(Day = 1:30, State = sim_states, WindSpeed = sim_speeds)
print(predicted_df)

a_df<-read.csv("C://Users//pc//OneDrive//msc statistics notes//2 sem//proj//actual.csv")
ac_df<-a_df[1:300,]
comparison_df <- data.frame(
  Day = 1:300,
  Actual = ac_df$wnd_speed,
  Predicted = predicted_df$WindSpeed
)
library(ggplot2)

ggplot(comparison_df, aes(x = Day)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2,linetype = "dashed" ) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(
    title = "Actual vs Predicted Wind Speed (30 Days)",
    x = "Day",
    y = "Wind Speed (m/s)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))






library(dplyr)

# Step 1: Clean data (remove Unclassified states)
data1 <- data %>% filter(wnd_state != "Unclassified")

# Step 2: Convert full state names to single-letter codes
data1 <- data1 %>%
  mutate(state_code = case_when(
    wnd_state == "Light" ~ "L",
    wnd_state == "Moderate" ~ "M",
    wnd_state == "Strong" ~ "S"
  ))

# Step 3: Build second-order transition probability table
transition_probs_2nd <- data1 %>%
  group_by(transition, state_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(transition) %>%
  mutate(prob = n / sum(n))

# Step 4: Simulation function (returns letter codes like L, M, S)
simulate_2nd_order_chain <- function(trans_prob_table, init_pair = "LL", n_days = 300) {
  predictions <- character(n_days)
  predictions[1] <- substr(init_pair, 1, 1)
  predictions[2] <- substr(init_pair, 2, 2)
  
  for (t in 3:n_days) {
    pair <- paste0(predictions[t - 2], predictions[t - 1])
    options <- trans_prob_table %>% filter(transition == pair)
    
    if (nrow(options) == 0) {
      predictions[t] <- predictions[t - 1]  # fallback to previous
    } else {
      predictions[t] <- sample(options$state_code, size = 1, prob = options$prob)
    }
  }
  
  return(predictions)
}

# Step 5: Map single-letter states to full names
map_state <- function(letter) {
  if (letter == "L") return("Light")
  if (letter == "M") return("Moderate")
  if (letter == "S") return("Strong")
  return(NA)
}

# Step 6: Compute average wind speed per state
means <- data1 %>%
  group_by(wnd_state) %>%
  summarise(avg_speed = mean(wnd_speed, na.rm = TRUE))

# Create lookup table for mean wind speed
speed_lookup <- setNames(means$avg_speed, means$wnd_state)

# Step 7: Predict future states and assign speeds
n_days <- 300  # Change this to 30 for short run
sim_codes <- simulate_2nd_order_chain(transition_probs_2nd, init_pair = "LL", n_days = n_days)
sim_states <- sapply(sim_codes, map_state)

# Option 1: Predict using mean speed per state
sim_speeds <- sapply(sim_states, function(s) speed_lookup[s])

# Option 2 (Optional): Add randomness (uncomment this to use sampling instead of mean)
# sim_speeds <- sapply(sim_states, function(s) {
#   speeds <- data1$wnd_speed[data1$wnd_state == s]
#   if (length(speeds) == 0) return(NA)
#   else return(sample(speeds, size = 1))
# })

# Step 8: Final predicted dataframe
predicted_df <- data.frame(
  Day = 1:n_days,
  State = sim_states,
  WindSpeed = sim_speeds
)

# View first few results
print(head(predicted_df, 10))
