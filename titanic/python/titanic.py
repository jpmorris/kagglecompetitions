#!/usr/bin/python

# Following the titanic kaggle competition guide

import csv as csv
import numpy as np

import os
os.chdir("/home/jmorris/workspace/kagglecompetitions/kaggle-titanicPython")

# --- Reading in your train.csv

# Open up the csv file in to a Python object
csv_file_object = csv.reader(open('./train.csv', 'rb'))
header = csv_file_object.next() # skip header

data=[]
for row in csv_file_object:
        data.append(row)
data = np.array(data)

# --- I have my data now I want to play with it

number_passengers = np.size(data[0::,1].astype(np.float))
number_survived = np.sum(data[0::,1].astype(np.float))
proportion_survivors = number_survived / number_passengers

women_only_stats = data[0::,4] == "female"
men_only_stats = data[0::,4] != "female"

women_onboard = data[women_only_stats,1].astype(np.float)
men_onboard = data[men_only_stats,1].astype(np.float)

proportion_women_survived = np.sum(women_onboard) / np.size(women_onboard)
proportion_men_survived = np.sum(men_onboard) / np.size(men_onboard)

print 'Proportion of women who survived is %s' % proportion_women_survived
print 'Proportion of men who survived is %s' % proportion_men_survived

# ---- Reading test data and writing the gender model as a csv

test_file = open('test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()

prediction_file = open("genderbasedmodel.csv", "wb")
prediction_file_object = csv.writer(prediction_file)

prediction_file_object.writerow(["PassengerId", "Survived"])
for row in test_file_object:
    if row[3] == 'female':
        prediction_file_object.writerow([row[0], '1'])
    else:
        prediction_file_object.writerow([row[0], '0'])
test_file.close()
prediction_file.close()

# --- Pythonizing the second submission

# So we add a ceiling
fare_ceiling = 40
# then modify the data in the Fare column to = 39, if it is greater or equal to the ceiling
data[ data[0::,9].astype(np.float) >= fare_ceiling, 9 ] = fare_ceiling - 1.0

fare_bracket_size = 10
number_of_price_brackets = fare_ceiling / fare_bracket_size

# I know there were 1st, 2nd and 3rd classes on board
number_of_classes = 3

# But it's better practice to calculate this from the data directly
# Take the length of an array of unique values in column index 2
number_of_classes = len(np.unique(data[0::,2]))

# Initialize the survival table with all zeros
survival_table = np.zeros((2, number_of_classes, number_of_price_brackets))


for i in xrange(number_of_classes):       #loop through each class
  for j in xrange(number_of_price_brackets):   #loop through each price bin

    women_only_stats = data[                          \
                         (data[0::,4] == "female")    \
                       &(data[0::,2].astype(np.float) \
                             == i+1) \
                       &(data[0:,9].astype(np.float)  \
                            >= j*fare_bracket_size)   \
                       &(data[0:,9].astype(np.float)  \
                            < (j+1)*fare_bracket_size)\
                          , 1]

    men_only_stats = data[                            \
                         (data[0::,4] != "female")    \
                       &(data[0::,2].astype(np.float) \
                             == i+1)                  \
                       &(data[0:,9].astype(np.float)  \
                            >= j*fare_bracket_size)   \
                       &(data[0:,9].astype(np.float)  \
                            < (j+1)*fare_bracket_size)\
                          , 1]

    survival_table[0,i,j] = np.mean(women_only_stats.astype(np.float))
    survival_table[1,i,j] = np.mean(men_only_stats.astype(np.float))


test_file = open('test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()
predictions_file = open("genderclassmodel.csv", "wb")
p = csv.writer(predictions_file)
p.writerow(["PassengerId", "Survived"])

for row in test_file_object:


  for j in xrange(number_of_price_brackets):

    try:

      row[8] = float(row[8])
    except:
      bin_fare = 3 - float(row[1])
      break
    if row[8] > fare_ceiling:


      bin_fare = number_of_price_brackets-1
      break
    if row[8] >= j * fare_bracket_size\
       and row[8] < \
       (j+1) * fare_bracket_size:

      bin_fare = j
      break
if row[3] == 'female':
        p.writerow([row[0], "%d" % \
                   int(survival_table[0, float(row[1])-1, bin_fare])])
else:
        p.writerow([row[0], "%d" % \
                   int(survival_table[1, float(row[1])-1, bin_fare])])

# Close out the files.
test_file.close()
predictions_file.close()