

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#import workout data from CSV
workoutdf = pd.read_csv('/Users/nickorangio/Main Directory/Python /PyCharm Documents/apple_watch_data/allWorkouts02-dec.csv', \
                        sep=',')

#examine workout data
#print(workoutdf.to_string())

#extract yoga workouts, drop excess columns, add month column
yogadf = workoutdf[workoutdf['Type'] == 'Yoga']
yogadf = yogadf.drop(['Distance','Average Pace','Average Speed'], axis=1)
yogadf['Start'] = pd.to_datetime(yogadf['Start'])
yogadf['Month'] = yogadf['Start'].dt.month
yogadf = yogadf.sort_values('Duration', ascending=True)
print(yogadf.to_string())

#line plot of total calories burned by date
#plt.plot(yogadf['Start'], yogadf['Total Energy kcal'])
#plt.show()

#scatterplot of total calories by duration
fig, ax = plt.subplots()
ax.scatter(yogadf['Duration'], yogadf['Total Energy kcal'], s = yogadf['Max Heart Rate'], c = yogadf['Month'], \
           alpha = 0.7)


#ax.scatter(yogadf['Average Heart Rate'], yogadf['Total Energy kcal'], s = yogadf['Duration'], c = yogadf['Month'], \
           #alpha = 0.6)

ax.set_xlabel('Duration', fontsize=15)
ax.set_ylabel('Total kcal burn', fontsize=15)
ax.set_title('Total calorie burn of yoga workouts by duration')
ax.tick_params(axis = 'x', rotation = 45)


ax.grid(True)
fig.tight_layout()

plt.show()