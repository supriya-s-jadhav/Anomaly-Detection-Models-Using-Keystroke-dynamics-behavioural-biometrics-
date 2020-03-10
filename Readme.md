# Anomaly Detection Models Using Keystroke Dynamics (Behavioral Biometrics)

How does a system identify users based on their typing rhythms? Keystroke dynamics is the analysis of typing rhythms to discriminate among users and it has been proposed for detecting impostors (i.e. both insiders and external attackers). Typing biometrics is part of a larger class of biometrics known as "behavioral biometrics" which uses different individual's typing pattern to authenticate individual person's identity.

The case study includes using a dataset which consists of typing speed information of a passcode by group of 51 different users recorded at different time intervals, building a classifier that recognizes typing pattern of different users, test the classifier on 5 different datasets having typing speed information of 5 different users and identify the right user.

# Problem Statement

The passcode dataset consists of passcode related information of 51 different users. It is a multi-class classification problem with 51 levels. We will be looking at two sets of data:

1. Known.csv
2. Set of 12 files with unknown users

(The set of 12 files are the test files used to test the prediction accuracy of the final classifier selected to predict the imposter. These files were give at the time of presentation by the professor Dr.Christopher Saunders at South Dakota State University. Hence, these files will not be made available in the repository.)

## And perform following tasks:

Task 1 : Design a model.
Task 2 : Predict the individual user.
Task 3 : Provide method's accuracy and reasons of selecting particular methods.

# Dataset brief introduction

Dataset consists of 1777 observations over 35 columns, which represents typing speed of 51 different individuals who have access to a passcode. Only one user can use system at a time.

```
Passcode used: .tie5Roanl

```

## Variables Information:

Subject
sessionIndex
Reps
Hold time
Up-Hold Time
Down-Down Time (DD time = Hold time + Up-Down time )

## Uderstanding Keystroke dynamics used for biometrics

![keystroke dynamics]()
# Applications:

1. Biometric Identification for user authentication.
2. Prevent Identity theft
3. As handwriting is used to identify the author of a written text, we can use typing pattern to distinguish people in a computer-based crime.
4. Access-control and authentication mechanism

# Advantages:

1. Improved security benefits.
2. More cost effective.
3. Continuous authentication strategies.