

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#import workout data from CSV created with "Workout" iOS app
workoutdf = pd.read_csv('/Users/nickorangio/Main Directory/Python /PyCharm Documents/apple_watch_data/allWorkouts01-jan-2018.csv', \
                        sep=',')

#pre-process data
yogadf = workoutdf[workoutdf['Type'] == 'Yoga'] #filter out yoga workouts
yogadf = yogadf.drop(['Distance','Average Pace','Average Speed'], axis=1) #drop excess columns
yogadf['Start'] = pd.to_datetime(yogadf['Start']) #convert to datetime format
yogadf['Month'] = yogadf['Start'].dt.month #create a month column
yogadf = yogadf.sort_values('Duration', ascending=True) #sort by workout duration
print(yogadf.to_string())

#scatterplot of total calories by duration
fig, ax = plt.subplots()
ax.scatter(yogadf['Duration'], yogadf['Total Energy kcal'], s = yogadf['Max Heart Rate'], c = yogadf['Month'], \
           alpha = 0.6, edgecolors = 'none', marker = 'o')


#ax.scatter(yogadf['Average Heart Rate'], yogadf['Total Energy kcal'], s = yogadf['Duration'], c = yogadf['Month'], \
           #alpha = 0.6)

ax.set_xlabel('Duration', fontsize=15)
ax.set_ylabel('Total kcal burn', fontsize=15)
ax.set_title('Total calorie burn of yoga workouts by duration', fontsize=15)
ax.tick_params(axis = 'x', rotation = 45)

ax.grid(True)
fig.tight_layout()

plt.show()