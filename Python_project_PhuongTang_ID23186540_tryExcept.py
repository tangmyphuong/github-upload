"""Program name: Stock classification.

Author: Phuong Tang
Last edited: 14/10/2019
Contact the developer: mpt54@uclive.ac.nz

Description: This program is to provide user an overview about their stock status 
and a tool to classify their stock into class A,B,C. Based on that, they can 
apply a proper purchase policy for Raw/Pack materials in each class. The value of this program is 
to give a fast overview about the results after classifying stock into ABC class 
as well as export report for Warehouse manager.

Note: use Spyder(Anaconda) to open the program. If you use Wing IDE101, pls click
the lady bug to start the program   

"""
import pandas as pd
import numpy as np
from pandas import DataFrame

class Descriptive_stats(object):
    """Defines the Descriptive_stats class"""   

    def __init__(self, filename):
        """define specific Descriptive_stats object with specific file                 
        """        
        self.filename = filename
        self.data = pd.read_csv(self.filename)
        
    def descriptive_stat(self):
        """Method: to calculate descriptive statistics sum, count, total, min, max"""
        count_data = self.data["Weekly spend (USD)"].count()
        sum_data = self.data["Weekly spend (USD)"].sum()
        mean_data = self.data["Weekly spend (USD)"].mean()
        max_data = self.data["Weekly spend (USD)"].max()
        min_data = self.data["Weekly spend (USD)"].min()
        stats = (count_data, sum_data, mean_data, max_data, min_data)
        return stats
    
    def __str__(self):
        """Returns the formatted string represent of the Descriptive_stats object"""
        stats = self.descriptive_stat()
        count_data = stats[0]
        sum_data = stats[1]
        mean_data = stats[2]
        max_data = stats[3]
        min_data = stats[4]
        template = "Total number of items: {0:.0f} items.\nTotal stock value: {1:.0f} USD.\nMean stock value: {2:.0f} USD.\nMax stock value: {3:.0f} USD.\nMin stock value: {4:.0f} USD."
        return template.format(count_data, sum_data, mean_data, max_data, min_data)
    
class ABC_classification(object):
    """Defines the ABC_classification class"""
    
    def __init__(self, filename, threshold_a, threshold_b):
        """define specific ABC_classification object with specific threshold                 
        """        
        self.filename = filename
        self.data = pd.read_csv(self.filename)
        self.threshold_a = threshold_a
        self.threshold_b = threshold_b
        
    def ABC_classification(self, cumulative_percentage):
        """Method: ABC classification rule"""   
        threshold_a = self.threshold_a
        threshold_b = self.threshold_b
        
        if cumulative_percentage <= self.threshold_a:
            self.result = "A"
        elif cumulative_percentage <= self.threshold_b + self.threshold_a:
            self.result = "B"
        else:
            self.result = "C"
        return self.result
            
    def new_dataframe(self):
        """Method: 
        Step 1: assign each item in the list to class A,B,C
        Step 2: create new data frame including new columns"""
        
        self.sum_data = self.data["Weekly spend (USD)"].sum()
        self.data = self.data.sort_values(by=["Weekly spend (USD)"], ascending=False)
        self.data["Cumulative spend"] = self.data["Weekly spend (USD)"].cumsum()
        self.data["Cumulative spending percentage"] = self.data["Cumulative spend"]/self.sum_data
        self.data["ABC_classification"] = self.data["Cumulative spending percentage"].apply(self.ABC_classification)
        self.df = DataFrame(self.data)
        return self.df
    
    def results(self):    
        """sum total weekly spend (total stock value) of each class"""
        
        result = []
        df = self.new_dataframe()
        for class_name in ["A", "B", "C"]:
            self.value_by_class = df[df["ABC_classification"] == class_name]["Weekly spend (USD)"].sum()
            result.append(self.value_by_class)
        return result
    
    def export_results(self):
        """export dataframe after ABC classification to file csv output"""
        df = self.new_dataframe()
        export_csv = df.to_csv("export_data_frame.csv")
        
    
    def __str__(self):
        """Returns the formatted string represent of the ABC_classification object"""
        
        value_by_class = self.results() #call the method
        sum_class_A = value_by_class[0]
        sum_class_B = value_by_class[1]
        sum_class_C = value_by_class[2]
        
        sum_data = self.data["Weekly spend (USD)"].sum()
        percent_classA = sum_class_A/sum_data*100
        percent_classB = sum_class_B/sum_data*100
        percent_classC = sum_class_C/sum_data*100
        
        df = self.new_dataframe()
        class_name = df["ABC_classification"].value_counts().keys().tolist()
        count_class = df["ABC_classification"].value_counts().tolist()
        
        template_1 = "Total weekly spend of class A: {0:.0f} USD\nTotal weekly spend of class B: {1:.0f} USD\nTotal weekly spend of class C: {2:.0f} USD"
        template_2 = "Percentage stock of class A: {0:.1f}%\nPercentage stock of class B: {1:.1f}%\nPercentage stock of class C: {2:.1f}%"
        template_3 = "Class {0}: {1} items\nClass {2}: {3} items\nClass {4}: {5} items"
        
        message_1 = template_1.format(sum_class_A, sum_class_B, sum_class_C)
        message_2 = template_2.format(percent_classA, percent_classB, percent_classC)
        message_3 = template_3.format(class_name[0], count_class[0], class_name[1], count_class[1], class_name[2], count_class[2])
        return message_1 + 2 * '\n' + message_2 + 2* "\n" + message_3             


# define user interface

import tkinter as tk
from tkinter import *
from tkinter.ttk import *
from tkinter import Frame

class StockGui(object):
    """define StockGui class"""
    def __init__(self, window):
        """Setup GUI on given window"""
        
        self.header_frame = Frame(window, bg='deepskyblue', width=600, height=70)
        self.header_frame.grid(row=0, column=0, columnspan=2)
            
        self.main_frame = Frame(window, bg='white', highlightbackground="white", highlightthickness=20, width=600, height=700)
        self.main_frame.grid(row=1, column=1, rowspan=2)
        
        # Header Frame
        self.header_frame.grid_propagate(0)
        self.header_frame.grid_rowconfigure(0, weight=1)
        self.header_frame.grid_columnconfigure(1, weight=1)
        
        self.label_header_text_2 = tk.Label(self.header_frame, text='ABC classification results', bg='deepskyblue', fg='white', font=("Arial", 28))
        self.label_header_text_2.grid(row=0, column=1)
        
        # Main Frame
        self.main_frame.grid_propagate(0)
        self.main_frame.grid_rowconfigure(1, weight=1)
        self.main_frame.grid_columnconfigure(1, weight=1)
        
        #Create 2 frames inside the main frame
        self.top_main_frame = Frame(self.main_frame, bg='white', highlightbackground="#5f9cad", highlightthickness=2, width=560, height=120)
        self.top_main_frame.grid(row=0)
        self.top_main_frame.grid_propagate(0)
        self.top_main_frame.grid_rowconfigure(0, weight=1)
        self.top_main_frame.grid_columnconfigure(2, weight=1)
        
        self.bottom_main_frame = Frame(self.main_frame, bg='white', width=560, height=580)    
        self.bottom_main_frame.grid(row=1, sticky='n')
        self.bottom_main_frame.grid_propagate(0)
        self.bottom_main_frame.grid_rowconfigure(0, weight=1)
        self.bottom_main_frame.grid_columnconfigure(2, weight=1)
        
        # create the widgets for the top_main_frame
        self.btn_stock_info = tk.Button(self.top_main_frame, text='See my stock status', command=self.stock_status)
        
        self.btn_stock_info.grid(row=0, column=2)
        
        self.label_stock_info = tk.Label(self.top_main_frame, text="", bg='white')
        self.label_stock_info.grid(row=1, column=2, sticky='n')
        
        # create the widgets for the bottom_main_frame
        self.label_threshold = tk.Label(self.bottom_main_frame, text= 'Please input your threshold for each class', bg='white')
        self.label_threshold.grid(row=0, column=0, pady=5)
        
        template_threshold = "For example: class A accounts for 70% of stock value (please input: 0.7),\nclass B accounts for 25% of stock value (input: 0.25) and class C accounts for the rest 5% of stock value"
        self.label_threshold_note = tk.Label(self.bottom_main_frame, text=template_threshold , bg='white')
        self.label_threshold_note.grid(row=1, column=0, pady=5)
        
        self.label_class_a = tk.Label(self.bottom_main_frame, text= 'Class A', bg='white')
        self.label_class_a.grid(row=2, column=0, pady=5)
        
        self.entry_class_a = tk.Entry(self.bottom_main_frame, width=20)
        self.entry_class_a.grid(row=3, column=0)    
        
        self.label_class_b = tk.Label(self.bottom_main_frame, text= 'Class B', bg='white')
        self.label_class_b.grid(row=4, column=0, pady=5)
        
        self.entry_class_b = tk.Entry(self.bottom_main_frame, width=20)
        self.entry_class_b.grid(row=5, column=0)
        
        self.label_class_c = tk.Label(self.bottom_main_frame, text= 'Class C: the rest', bg='white')
        self.label_class_c.grid(row=6, column=0, pady=5)
        
        self.btn_ABC_classification = tk.Button(self.bottom_main_frame, text='Stock classification', command=self.ABC_results)
        self.btn_ABC_classification.grid(row=7, column=0, pady=5)
        
        self.label_result = tk.Label(self.bottom_main_frame, text= 'Results for stock classification:', bg='white')
        self.label_result.grid(row=8, column=0, pady=5)
        
        self.label_message = tk.Label(self.bottom_main_frame, text= '', bg='white', anchor=W, justify=LEFT)
        self.label_message.grid(row=9, column=0)
        
        self.btn_savefile = tk.Button(self.bottom_main_frame, text='Export ABC classification file', command=self.save_file)
        self.btn_savefile.grid(row=16, column=0, pady=5)        
        
        self.btn_plotgraph = tk.Button(self.bottom_main_frame, text='View Pareto chart', command=self.plot_graph)
        self.btn_plotgraph.grid(row=17, column=0, pady=10)        
      
        
    def stock_status(self):
        """The event handler for clicks on the button stock_status
        The purpose: to see current stock status based on descriptive analysis"""
        stock_info = Descriptive_stats("Data_stock.csv")
        self.label_stock_info.configure(text = stock_info)
        
    def ABC_results(self):
        """The event handler for clicks on the ABC results
        Purpose: to get threshold values from user input and print results after classification
        This event handler also handling Exception in case user input wrong"""
        
        if float(self.entry_class_a.get()) >= 1 or float(self.entry_class_b.get()) > 1:
            self.label_message.configure(text = "ERROR: Threshold value must less than 1!", fg='red')
        elif float(self.entry_class_a.get()) + float(self.entry_class_b.get()) > 1:
            self.label_message.configure(text = "ERROR: Sum of threshold values for class A, B, C must be equal to 1!", fg='red')
        else:
            ABC_stock = ABC_classification("Data_stock.csv", float(self.entry_class_a.get()), float(self.entry_class_b.get()))
            self.label_message.configure(text = ABC_stock, fg='blue')
        
    def save_file(self):
        """The event handler for clicks on the button save file
        The output will be a new Excel file with result of ABC classification for each item"""
        
        ABC_stock = ABC_classification("Data_stock.csv", float(self.entry_class_a.get()), float(self.entry_class_b.get()))
        ABC_stock.export_results()
    
    def plot_graph(self):
        """The event handler for clicks on the button plot_graph
        Purpose: to plot the Pareto chart
        """
        
        import matplotlib.pyplot as plt
        ABC_stock = ABC_classification("Data_stock.csv", float(self.entry_class_a.get()), float(self.entry_class_b.get()))
        df = ABC_stock.new_dataframe()
        x = np.arange(0, 416)
        y = df["Cumulative spending percentage"]
        plt.plot(x, y)
        plt.xlabel('Number of items')
        plt.ylabel('Weekly spending')
        plt.title("Pareto chart about Accumulated amount of value")
        plt.legend()
        plt.grid(True)
        plt.show()    
         
    
                         
def main():
    """Set up the GUI and run it."""
    window = tk.Tk()
    stock_gui = StockGui(window)
    window.mainloop()

main()
    

    



