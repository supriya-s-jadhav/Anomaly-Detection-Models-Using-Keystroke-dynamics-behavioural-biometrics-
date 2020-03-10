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

![keystroke dynamics](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/keystrokedynamics.png)

# Applications:

1. Biometric Identification for user authentication.
2. Prevent Identity theft
3. As handwriting is used to identify the author of a written text, we can use typing pattern to distinguish people in a computer-based crime.
4. Access-control and authentication mechanism

# Advantages:

1. Improved security benefits.
2. More cost effective.
3. Continuous authentication strategies.

# Dataset Exploration

1.Subjects
Brief look up at number of reps in each class.

![Frequency distribution](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-2-1.png)

2.Subjects' frequency by session Index
All users participate in session 8 but fewer users do not participate in session 7.

![Frequency distribution by session index 7](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-3-1.png)

## Hypothesis testing

Does sessionIndex makes any difference in measurements of passcode for users?

We performed independence test:

<b>Null hypothesis:</b> The true mean of a user in session 7 and session 8 are same.

<b>Alternative hypothesis:</b> The true mean of a user in session 7 and session 8 are different.

We found out that most of the p-values are greater than 0.05. Below are the results:

<b>Conclusion:</b> Fail to reject the null hypothesis at 0.05 significance level.. Hence we conclude that session index does not make any difference in individual user's passcode typing measurements.

Results:
![Hypothesis results](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/hypothesis%20testing.png)

## Distribution Graph

Let us look at the distribution of hold time for 3 users:
s003, s004, s005.

![Comparing 1](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-8-1.png)

![Comparing 2](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-8-2.png)

Let us look at the distribution of all user's hold time for typing passcode.

![Comparing all 1](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-9-1.png)

![Comparing all 2](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-9-2.png)

## Scatter plot:

Hold time and Up-Down time

![7.1](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-7-1.png)

![7.2](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-7-2.png)

![7.3](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-7-3.png)

## Box-plots

![12.1](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-12-1.png)

![12.2](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-12-2.png)

![12.3](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-12-3.png)

![12.4](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-12-4.png)

## Correlation Matrix

Looking at Correlation Matrix of variables: UD and DD variables are highly correlated.

![Correlation matrix](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-11-1.png)

## Histogram

![4.1](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-4-1.png)

![4.2](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-4-2.png)

![4.3](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-5-1.png)

![4.4](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/unnamed-chunk-5-2.png)

## Variable Selection

Variables Removed:

X, Rep, SessionIndex, DD variables-highly correlated with other variables and highly skewed

# Models

## Data Partition

Using 'createDataPartition' method that creates balanced splits of the data. The random sampling occurs within each class and it preserves the overall class distribution of the data.

![data partition](https://github.com/supriya-s-jadhav/Anomaly-Detection-Models-Using-Keystroke-dynamics-behavioural-biometrics-/blob/master/Images/data%20partition.png)

## Building Models

1. Multinomial Logistic Model

| Model Name | Method | Accuracy |
|------------|--------|----------|
|Multinomial Logistic Method| Train | 84.89%|
| | Test | 77.68% |

Test data: 95% Confidence Interval
    (74.39, 80.74)
