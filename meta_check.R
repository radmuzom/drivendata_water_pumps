library(data.table)

s1 <- fread("Submissions/Submission_20200319.csv")
s2 <- fread("Submissions/Submission_20200405.csv")
s3 <- fread("Submissions/Submission_20200405_v2.csv")

idx_mis <- which(s1$status_group != s2$status_group)
s1[idx_mis[1]]
s2[idx_mis[1]]
s3[idx_mis[1]]

cnt <- 0
for(i in 1:length(idx_mis)) {
  if (s3$status_group[idx_mis[i]] == s1$status_group[idx_mis[i]]) {
    cnt <- cnt + 1
    s2[idx_mis[i], "status_group"] <- s3$status_group[idx_mis[i]]
  }
}
cnt
fwrite(s2, "Submissions/Submission_20200405_v3.csv")

s4 <- fread("Submissions/Submission_20200405_v3.csv")
s2 <- fread("Submissions/Submission_20200405.csv")
all.equal(s2, s4)
