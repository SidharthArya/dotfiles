#!/usr/bin/env python3

import pandas as pd
# import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import os
import json

years = mdates.YearLocator()   # every year
months = mdates.MonthLocator()  # every month
day = mdates.DayLocator()  # every day
week = mdates.WeekdayLocator()  # every week
years_fmt = mdates.DateFormatter('%D')
#current_datelocator_minor_default = day
current_datelocator_major_default = week

def setdatelocators(ax):
    ax.xaxis.set_major_locator(current_datelocator_major_default)
    ax.xaxis.set_major_formatter(years_fmt)
#    ax.xaxis.set_minor_locator(current_datelocator_minor_default)

track = "/home/arya/Documents/Org/Tracking/"
limits = dict()
with open(track + "index") as fp:
    for line in fp:
        line=line.strip().split("=")
        limits[line[0].strip()] = json.loads(line[1])

        
for i in os.listdir(track):
    if not i.endswith('.csv'):
        continue
    print(i)
    current=pd.read_csv(track + i).dropna().sort_values(by="Date")
    for j in list(current.columns):
        if j != "Date":
            ax = plt.subplot(1, 1, 1)
            setdatelocators(ax)
            plt.plot(current["Date"], current[j])
            plt.title(i)
            try:
                ax.fill_between(current["Date"], limits[i[:-4]][x][0],limits[i[:-4]][x][1], alpha=0.3)
                x = x+1
            except Exception as e:
                pass
        else:
            current["Date"] = pd.to_datetime(current["Date"], format='%d-%m-%Y')
            current.sort_values(by="Date", inplace=True)
            x = 0

    plt.savefig(track + 'images/' + i + '.png')
    plt.clf()
