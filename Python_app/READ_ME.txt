Guidance for running program "Stock classification"

Author: Phuong Tang
Last edited: 14/10/2019

1. If you open the program by Spyder (Anaconda), it's fine.
If you open the program by Wing IDE 101, you need to click the "Lady bug" button to start the program rather than Run button. 
All functions in program will perform the same with Run function, so don't worry.
Reason: there's a clash between pandas and tkinter in Wing.

2. The source of data is in file "Data_stock.csv". Make sure you save this source file the same folder with the Python Program file.
It's also need to install Pandas through Command Prompt before running the Program by script "pip install pandas"

3. Other steps are follow the flow of GUI tkinter window:

+ Step 1: Click the button "See my stock status" to view some descriptive statistics of the stock data like: count the number of items, 
sum of total stock value, mean stock value (the average stock value), max stock (the value of the most expensive item), min stock (the value of the cheapest item).

If the user have new items, they can update the list of these items in "Data_stock.csv", then these descriptive statistics parameters will change.

+ Step 2: For ABC classification: require user to input the threshold of each class.  
For example: class A accounts for 70% of stock value (please input: 0.7), class B accounts for 25% of stock value (input: 0.25)
Class C is extrapolated by (1 - 0.7 - 0.25) = 0.05 ~ 5% of stock value

Then click the button "Stock classification" to see the results after classifying stock.

In this case, if the user input threshold larger than 1, the ERROR will be appeared in red color ("ERROR: Threshold value must less than 1!")
If the user input threshold class A and threhold class B smaller than 1 but the sum of these threshole is greater than 1, the ERROR will be appeared in red color 
("ERROR: Sum of threshold values for class A, B, C must be equal to 1!")

+ Step 3: If the user want to export data to csv file, click the button "Export ABC classification file".
One csv file named "export_data_frame.csv" will be appeared in the same folder of the Program file.
Many new columns added in this file, including the column named "ABC_classification". The warehouse manager or the planning manager 
can based on this file to know which item is particularly classified to which class and have the proper purchase policy of each item.

+ Step 4: click the button "View Pareto chart" to view the Pareto chart of ABC classification.

