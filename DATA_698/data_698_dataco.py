# -*- coding: utf-8 -*-
"""DATA_698_DataCo.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1cKzkjrrp0R_u4phK8KRp2nVYKuFeE8ie

**DATA 698**


---


E-Commerce website traffic analysis using Graph Neural Networks

Data Exploration on the dataset from DataCo and related files

Importing the libraries
"""

from psutil import virtual_memory
ram_gb = virtual_memory().total / 1e9
print('Your runtime has {:.1f} gigabytes of available RAM\n'.format(ram_gb))

if ram_gb < 20:
  print('Not using a high-RAM runtime')
else:
  print('You are using a high-RAM runtime!')

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
#to ignore warnings
import warnings
warnings.filterwarnings('ignore')

"""Importing the dataset"""

# Access Log data
access_log_url = 'https://raw.githubusercontent.com/baruab/baruab/refs/heads/main/DATA_698/DataCo/tokenized_access_logs.csv'
df_ac_log = pd.read_csv(access_log_url)
df_ac_log.head()

# Supply Chain data
#sc_url = 'https://raw.githubusercontent.com/baruab/baruab/refs/heads/main/DATA_698/DataCo/DataCoSupplyChainDataset.csv'
#df_sc = pd.read_csv(sc_url , encoding='latin1')
#df_sc.head()

df_ac_log.info()

#df_sc.info()

#df_ac_log.describe()

print(min(df_ac_log['Date']))
print(max(df_ac_log['Date']))

#df_sc.describe()

"""Check for Duplication"""

df_ac_log.duplicated().sum()

"""Unique values in Access Log"""

df_ac_log.nunique()

#df_sc.nunique()

"""Check for missing values in dataset"""

df_ac_log.isnull().sum()

#df_sc.isnull().sum()

"""**Feature Engineering**

Add geo features to the data based on IP address information. Map the IP address with Country, State and City.
"""

#!pip install ipinfo
#import ipinfo
#import json
#import requests

#json_url = 'https://raw.githubusercontent.com/baruab/baruab/refs/heads/main/DATA_698/DataCo/iso-3166-2.json'

#try:
#    response = requests.get(json_url)  # Fetch the JSON data from the URL
#    response.raise_for_status()  # Raise an exception for bad responses
#    state_abbr_data = json.loads(response.text)  # Load JSON from the response text
#except requests.exceptions.RequestException as e:
#    print(f"Error fetching JSON data: {e}")


#access_token = '8f8f1378474839'  # Optional, but added for more data  8f8f1378474839
#handler = ipinfo.getHandler(access_token)
#count = 0 # Added to tally the number

#def get_state_abbr(country_code, state_name):
#    country_data = state_abbr_data.get(country_code, {})
#    divisions = country_data.get("divisions", {})

#   for abbr, full_name in divisions.items():
#        if full_name.lower() == state_name.lower():
#            return abbr.split('-')[-1]

#    return " - "

#def get_location(ip):
#    global count
#    try:
#        count += 1
#        details = handler.getDetails(ip)

#        city = details.city if details.city else None
#        country = details.country if details.country else None  # ISO country code
#        country_name = details.country_name if details.country_name else None
#        state = details.region if details.region else None

#        if state and country:
#            state_abv = get_state_abbr(country, state)

 #       print(f"Processing IP {count}: {ip} -> City: {city}, Country: {country}, State: {state}")
 #       return city, country_name, state, country, state_abv
 #   except Exception as e:
 #       print(f"Exception for IP {ip}: {e}")
 #       return None, None, None, None, None


## In the interest of saving time, calling the API multiple times, as the information is of static nature
##   the generated file with added columns is saved and read directly to save compute time

# df_ac_log['City'], df_ac_log['Country'], df_ac_log['State'],  df_ac_log['Country_Code'], df_ac_log['State_Code'] = zip(*df_ac_log['ip'].apply(get_location))
#output_file = 'access_logs_with_state_cntry.csv'
#df_ac_log.to_csv(output_file, index=False)
#df_ac_log.head()

#access_log_location_url = 'https://raw.githubusercontent.com/baruab/baruab/refs/heads/main/DATA_698/access_logs_with_location_codes.csv'
#df_ac_log = pd.read_csv(access_log_location_url)

## In the interest of saving time, calling the API multiple times, as the information is of static nature
##   the generated file with added columns is saved and read directly to save compute time
## Note: The file was truncated in size >100 KB, filtered for US only to upload in Github

access_log_location_url = 'https://raw.githubusercontent.com/baruab/baruab/refs/heads/main/DATA_698/tokenized_access_logs_global.csv'
df_ac_log = pd.read_csv(access_log_location_url)

"""Feature Engineering

Let's split the date time string into date & time represented as numbers to create indexes, if needed
"""

df_ac_log['date_id'] = df_ac_log['Date'].str.split('/').str[1]
df_ac_log['month_id'] = df_ac_log['Date'].str.split('/').str[0]
df_ac_log['year_id'] = df_ac_log['Date'].str.split('/').str[2].str.split(' ').str[0]

df_ac_log['time'] = df_ac_log['Date'].str.split(' ').str[1]

#df_ac_log.head()

print(len(df_ac_log))

"""Converting Date to add Day Of the Week in Access Log"""

df_ac_log['Dt'] = df_ac_log['Date'].str.split(' ').str[0]

# Convert the 'date' column to datetime
df_ac_log['date'] = pd.to_datetime(df_ac_log['Dt'])

# Extract the day of the week
df_ac_log['day_of_week'] = df_ac_log['date'].dt.day_name()
df_ac_log['weekday'] = df_ac_log['date'].dt.weekday
#df_ac_log.head()

"""**Data Sorting and Grouping**

Sort the access log by IP address, date to understand the sequence of web clicks
"""

df_ac_log.sort_values(['ip'], ascending=[True])  #, 'date', 'time'

#df_ac_log.head(30)

#df_ac_log.groupby(['ip']).count()

"""Let's seperate Numerical and Categorical variables"""

cat_cols = df_ac_log.select_dtypes(include=['object']).columns.tolist()
num_cols = df_ac_log.select_dtypes(include=np.number).columns.tolist()
print("Categorical Variables:")
print(cat_cols)
print("Numerical Variables:")
print(num_cols)

"""Converting string number values to integers"""

# Convert 'numbers' column to integer
df_ac_log['date_id'] = df_ac_log['date_id'].astype(int)
df_ac_log['month_id'] = df_ac_log['month_id'].astype(int)
df_ac_log['year_id'] = df_ac_log['year_id'].astype(int)
num_cols = df_ac_log.select_dtypes(include=np.number).columns.tolist()
print("Numerical Variables:")
print(num_cols)

"""EDA Univariate Analysis

Below are some histogram and box plots showing the pattern of variables using the Access Log information
"""

for col in num_cols:
  print(col)
  print('Skew:', round(df_ac_log[col].skew(),2))
  print('Kurtosis:', round(df_ac_log[col].kurtosis(),2))

  plt.figure(figsize=(15,4))
  plt.subplot(1,2,1)
  sns.histplot(data=df_ac_log, x=col, kde=True)
  plt.subplot(1,2,2)
  sns.boxplot(data=df_ac_log, x=col)
  plt.show()

"""The month value is left skewed, the access traffic spikes during mid of the week and mid of the month."""

fig, axes = plt.subplots(6,1, figsize=(36,36))
fig.suptitle('Bar plot for all categorical variables in the dataset')

sns.countplot(ax = axes[0], x = 'Product', data = df_ac_log, color='blue',
              order=df_ac_log['Product'].value_counts().index);
sns.countplot(ax = axes[1], x = 'Category', data = df_ac_log, color='blue',
              order=df_ac_log['Category'].value_counts().index);
sns.countplot(ax = axes[2], x = 'Department', data = df_ac_log, color='blue',
              order=df_ac_log['Department'].value_counts().index);
sns.countplot(ax = axes[3], x = 'City', data = df_ac_log, color='blue',
              order=df_ac_log['City'].value_counts().index);
sns.countplot(ax = axes[4], x = 'State', data = df_ac_log, color='blue',
              order=df_ac_log['State'].value_counts().index);
sns.countplot(ax = axes[5], x = 'Country', data = df_ac_log, color='blue',
              order=df_ac_log['Country'].value_counts().index);

axes[1].tick_params(labelrotation=45);
axes[2].tick_params(labelrotation=90);
axes[3].tick_params(labelrotation=90);

axes[4].tick_params(labelrotation=90);
axes[5].tick_params(labelrotation=90);
plt.show()

"""Looking at the chart above, Product Category, Department, State are relatively fewer unique values, so worth looking into further.

**Data Transformation**

"""

## 'Hour', 'date_id', 'month_id', 'year_id', 'weekday'
## The numerical values are skewed, log transformation can help in normalization

# Function for log transformation of the column
def log_transform(data, col):
  for colname in col:
    if (data[colname] == 1.0).all():
      data[colname + '_log'] = np.log(data[colname] + 1)
    else:
      data[colname + '_log'] = np.log(data[colname])
  #data.info()

log_transform(df_ac_log, ['Hour', 'date_id', 'month_id', 'weekday'])


# Replacing inf with a large finite value and -inf with a small finite value
df_ac_log.replace([np.inf, -np.inf], [np.finfo(np.float64).max, np.finfo(np.float64).min], inplace=True)


#Log transformation of the features
#sns.distplot(df_ac_log['Hour_log'], axlabel='Hour_log')
sns.distplot(df_ac_log['date_id_log'], axlabel='date_id_log')
sns.distplot(df_ac_log['month_id_log'], axlabel='month_id_log')
#sns.distplot(df_ac_log['weekday_log'], axlabel='weekday_log')
plt.show()

"""**Encoding the categorical variables**

Unique Elements in Categorical Columns
"""

print(len(df_ac_log))

df_ac_log.info()

##df_ac_US_log= df_ac_log

"""
print('Product: ' + df_ac_US_log['Product'].unique())
print('Category: ' + df_ac_US_log['Category'].unique())
print('Department: ' + df_ac_US_log['Department'].unique())
print('City: ' + df_ac_US_log['City'].unique())
print('State: ' + df_ac_US_log['State'].unique())
print('Country: ' + df_ac_US_log['Country'].unique())
"""

df_ac_log['ip'].value_counts()

#
# Access Log of Top 5 IP address to be selected

top_ips = df_ac_log['ip'].value_counts().head(5)

#print(top_ips.index.tolist())

df_top_ip_access = df_ac_log[df_ac_log.ip.isin(top_ips.index.tolist())]

#df_top_ip_access.head(10)

"""Top 5 IP address access breakdown by date and the buying intent"""

print(len(df_top_ip_access))
df_top_ip_access.info()

#df_top_ip_access.value_counts(['ip', 'date','AddToCart'])

df_top_ip_access['Product'].value_counts()

df_ac_log['Department'].value_counts()

#df_ac_US_log['State'].value_counts()

print(min(df_ac_log['Dt']))
print(max(df_ac_log['Dt']))

"""As there are a lot of categorical columns with many uniques values, let's subset the dataframe"""

df_ac_cat_subset = df_ac_log[["Department", "Dt", "State"]]

df_ac_cat_subset['Dt'].value_counts()

print(len(df_ac_cat_subset))

#one hot encoding using OneHotEncoder of Scikit-Learn

from sklearn.preprocessing import OneHotEncoder


#Extract categorical columns from the dataframe
#Here we extract the columns with object datatype as they are the categorical columns
categorical_columns = df_ac_cat_subset.select_dtypes(include=['object']).columns.tolist()

#Initialize OneHotEncoder
encoder = OneHotEncoder(sparse_output=False)

# Apply one-hot encoding to the categorical columns
one_hot_encoded = encoder.fit_transform(df_ac_cat_subset[categorical_columns])

#Create a DataFrame with the one-hot encoded columns
#We use get_feature_names_out() to get the column names for the encoded data
one_hot_df = pd.DataFrame(one_hot_encoded, columns=encoder.get_feature_names_out(categorical_columns))

# Concatenate the one-hot encoded dataframe with the original dataframe
df_encoded = pd.concat([df_ac_cat_subset, one_hot_df], axis=1)

# Drop the original categorical columns
df_encoded = df_encoded.drop(categorical_columns, axis=1)

# Display the resulting dataframe
print(f"Encoded data : \n{df_encoded}")

"""**Tabular data -> Heterogeneous Graph Data**

Identify:
Nodes (Product, User, Location ...)
Edges (Interactions)
Node features(Attributes)
Label (Node level, Edge level, Graph level)
Timesteps (Interval time)
Temporal graph shape: static or dynamic? What is changing over time?

Identifying buying intent in the access log by searching for 'add_to_cart' related url links. This will be an edge feature in the graph.
"""

df_top_ip_access["AddToCart"] = df_top_ip_access["url"].str.contains("add_to_cart").astype(int) # str.extract("(add_to_cart)")
#df_top_ip_access.head()

# Let's take a subset of the dataframe with IP's with most access to the website df_top_ip_access

#df_top_ip_access.head()

# Reassign the IP address to IDs (make it easier later for creating edges)

ipaddrs = df_top_ip_access['ip'].unique()
new_ip_ids = list(range(len(df_top_ip_access['ip'].unique())))
map_ip = dict(zip(ipaddrs, new_ip_ids))
print(type(map_ip))

df_top_ip_access['ip_id'] = df_top_ip_access['ip'].map(map_ip)

#df_top_ip_access.head()

# Reassign the Product to IDs (make it easier later for creating edges)

products = df_top_ip_access['Product'].unique()
new_prod_ids = list(range(len(df_top_ip_access['Product'].unique())))
map_prod = dict(zip(products, new_prod_ids))
print(type(map_prod))

df_top_ip_access['Product_Id'] = df_top_ip_access['Product'].map(map_prod)

df_top_ip_access.head()

#df_top_ip_access.info()

"""**Based on the dataset, this will be a Heterogenous Graph comprising of Users(IP address) nodes and Product nodes, the edges will be represented as the buy intend (add_to_cart attribute)**


"""

#Let's start with the Product Node, create a subset dataframe.
#It will have Product_Id, Category, Department, url

df_product_nodes = df_top_ip_access[['Product_Id', 'Category']] #, 'Department', 'url']]
df_product_nodes.head()

# select Product node features
df_product_nodes = df_product_nodes.drop_duplicates()

df_product_nodes = df_product_nodes.reset_index(drop=True)
df_product_nodes.head()
print(len(df_product_nodes))

# Create a dictionary to store node features
node_features = {}

# Iterate through the rows of the dataframe
for index, row in df_product_nodes.iterrows():
    # Get the product ID
    product_id = row['Product_Id']
    # Create a feature vector for the node
    features = {
        'Category': row['Category']
    }
    # Store the feature vector in the dictionary
    node_features[product_id] = features

# Print the node features
#print(node_features)

# Access the features for a specific product ID
print(node_features[1])

#one hot encoding the node feature using OneHotEncoder of Scikit-Learn

from sklearn.preprocessing import OneHotEncoder


#Extract categorical columns from the dataframe
#Here we extract the columns with object datatype as they are the categorical columns
categorical_columns = df_product_nodes.select_dtypes(include=['object']).columns.tolist()

#Initialize OneHotEncoder
encoder = OneHotEncoder(sparse_output=False)

# Apply one-hot encoding to the categorical columns
one_hot_encoded = encoder.fit_transform(df_product_nodes[categorical_columns])

#Create a DataFrame with the one-hot encoded columns
#We use get_feature_names_out() to get the column names for the encoded data
one_hot_df = pd.DataFrame(one_hot_encoded, columns=encoder.get_feature_names_out(categorical_columns))

# Concatenate the one-hot encoded dataframe with the original dataframe
df_encoded = pd.concat([df_product_nodes, one_hot_df], axis=1)

# Drop the original categorical columns
df_product_features = df_encoded.drop(categorical_columns, axis=1)

# Display the resulting dataframe
print(f"Encoded data : \n{df_product_features}")

df_product_features.head()

# Convert to numpy
x = df_product_features.to_numpy()
print(x)
print(x.shape)

"""**Create User Node and Features**"""

#Let's create the User Node, create a subset dataframe.
# User features can be their access count and buy intend count

access_count = df_top_ip_access.groupby("ip_id")["date_id"].count().rename("access_count")
buy_count = df_top_ip_access[df_top_ip_access["AddToCart"] == 1].groupby("ip_id")["AddToCart"].count().rename("buy_count")
user_node_features = pd.concat([access_count, buy_count], axis=1)

# Remap user ID
user_node_features = user_node_features.reset_index(drop=False)
user_node_features.head()
user_id_mapping = user_node_features['ip_id']

# Only keep user features
user_node_features = user_node_features.drop('ip_id', axis=1)
user_node_features.head()

user_id_mapping.head()

# Convert to numpy
x = user_node_features.to_numpy()
print(x)
print(x.shape)

user_node_features["buy_count"].hist()

"""Creating the Edge index"""

!pip install torch # Install the PyTorch library
import torch # Import the torch module

## edge_index where AddToCart == 1
df_buy_edge= df_top_ip_access[df_top_ip_access["AddToCart"] == 1]
df_buy_edge.head()

edge_index = df_buy_edge[["ip_id", "Product_Id"]].values.transpose()
edge_index = torch.tensor(edge_index, dtype=torch.long)
print(edge_index)
print(edge_index.shape)

"""**Build the Heterogeneous graph data object**"""

!pip install torch_geometric

from torch_geometric.data import HeteroData

data = HeteroData()

# Set the number of nodes for 'user' and 'product'
data['user'].num_nodes = len(user_node_features) # Assuming user_node_features is a list or array of user features
data['product'].num_nodes = len(df_product_features) # Assuming df_product_features is a DataFrame or array of product features


data['user'].x = user_node_features
data['product'].x = df_product_features
data['user'].y = torch.tensor(df_top_ip_access['AddToCart'].values, dtype=torch.long)
data['user'].train_mask = torch.ones(len(user_node_features), dtype=torch.bool)
data['user'].test_mask = torch.zeros(len(user_node_features), dtype=torch.bool)
data['user'].val_mask = torch.zeros(len(user_node_features), dtype=torch.bool)




data['user', 'buy', 'product'].edge_index = edge_index

clear_output = True

print(type(data))
print(data)

### Visualize using NetworkX

# Simple example of network x rendering with colored nodes and edges
import matplotlib.pyplot as plt
import networkx as nx
from torch_geometric.utils import to_networkx

graph = to_networkx(data)

# Define colors for nodes and edges
node_type_colors = {
    "user": "#4599C3",
    "product": "#ED8546",
}

node_colors = []
labels = {}
for node, attrs in graph.nodes(data=True):
    node_type = attrs["type"]
    color = node_type_colors[node_type]
    node_colors.append(color)
    if attrs["type"] == "user":
        labels[node] = f"U{node}"
    elif attrs["type"] == "product":
        labels[node] = f"P{node}"

# Define colors for the edges
edge_type_colors = {
    ("user", "buy", "product"): "#8B4D9E",
    ("user", "view", "product"): "#DFB825",
}

edge_colors = []
for from_node, to_node, attrs in graph.edges(data=True):
    edge_type = attrs["type"]
    color = edge_type_colors[edge_type]

    graph.edges[from_node, to_node]["color"] = color
    edge_colors.append(color)


# Draw the graph
pos = nx.spring_layout(graph, k=2)
nx.draw_networkx(
    graph,
    pos=pos,
    labels=labels,
    with_labels=True,
    node_color=node_colors,
    edge_color=edge_colors,
    node_size=600,
)
plt.show()

"""**Measuring parameters in a PyTorch Geometric (PyG) HeteroData graph**"""

import torch
import torch.nn as nn
from torch_geometric.nn import SAGEConv, HeteroConv

def count_parameters(model):
    """Counts the total number of trainable parameters in a PyTorch model.
    """
    return sum(p.numel() for p in model.parameters() if p.requires_grad)

class HeteroGNN(nn.Module):
    def __init__(self, data):
        super().__init__()
        self.convs = nn.ModuleDict()
        # Create a mapping from node type names to numerical indices
        self.node_type_to_index = {node_type: i for i, node_type in enumerate(data.node_types)}

        for edge_type in data.edge_types:
            src_node_type, _, dst_node_type = edge_type
            # Convert edge_type to a string
            edge_type_str = ':'.join(edge_type)
            # Use numerical indices for in_channels
            self.convs[edge_type_str] = SAGEConv(
                in_channels={self.node_type_to_index[src_node_type]: data[src_node_type].x.shape[1],
                             self.node_type_to_index[dst_node_type]: data[dst_node_type].x.shape[1]},
                out_channels=64
            )

    def forward(self, data):
        x_dict = data.x_dict
        for edge_type, conv in self.convs.items():
            # Convert edge_type string back to tuple for data access
            edge_type_tuple = tuple(edge_type.split(':'))
            x_dict = conv(x_dict, data[edge_type_tuple].edge_index)
        return x_dict

#data = ...  # Your HeteroData object
model = HeteroGNN(data)

# Count parameters
# Assuming you have a function 'count_parameters' defined
total_params = count_parameters(model)
print(f"Total parameters: {total_params}")


# Iterate Over Edge Types
for edge_type in data.edge_types:
    # Access edge features for this type
    edge_index = data[edge_type].edge_index

    # Check if edge attributes exist before accessing them
    if 'edge_attr' in data[edge_type]:  # Check if edge_attr is present
        edge_features = data[edge_type].edge_attr
        print(f"Edge features for {edge_type}: {edge_features.shape}")
    else:
        print(f"No edge features found for {edge_type}")

    print(f"Edge index for {edge_type}: {edge_index.shape}")

"""# **Now our tabular data is ready to be used for Heterogenous Link-level GNN processing.**

**Preparing Data to create the  Heterogeneous Temporal Graph**
"""

# Order by date
df_top_ip_access = df_top_ip_access.sort_values(by='date')

# max date
max_date = df_top_ip_access['date'].max()
print('Max date: ' + str(max_date))

#min date
min_date = df_top_ip_access['date'].min()
print('Min date: ' + str(min_date))

# Split the data into daily access buckets
from datetime import datetime, timedelta
print(type(df_top_ip_access['date']))
df_top_ip_access['date']= pd.to_datetime(df_top_ip_access['date'])

start_date = pd.to_datetime(min_date)
end_date = pd.to_datetime(max_date)

interval = timedelta(days=1)
bucket_elements=[]

while start_date <= end_date:
  bucket_elements.append(df_top_ip_access[(start_date + interval) == df_top_ip_access["date"]].shape[0])
  start_date += interval

print(bucket_elements)

sns.scatterplot(x="index", y="access per day", data=pd.DataFrame(bucket_elements, columns=["access per day"]).reset_index())
plt.show()

#df_top_ip_access.head()

# Merge date and ip columns to create new column in df_top_ip_access
df_top_ip_access['date_ip'] = df_top_ip_access['date'].astype(str) + '_' + df_top_ip_access['ip'].astype(str)
#df_top_ip_access.head()

# Split the dataframe by date and ip address simulating temporal behaviour of the users on the website

def split_dataframe_by_column(df, column_name):
    """Splits a DataFrame into an array of DataFrames based on the unique values in a specified column.

    Args:
        df: The DataFrame to split.
        column_name: The name of the column to split by.

    Returns:
        A list of DataFrames.
    """

    dataframes = []
    for value in df[column_name].unique():
        dataframes.append(df[df[column_name] == value])
    return dataframes

split_dfs = split_dataframe_by_column(df_top_ip_access, 'date_ip')
print(len(split_dfs))
#print(split_dfs[1].info())

"""We have now split the dataset into user session datasets.

Let's calculate the average user request duration by session, # of requests per session, entry Product, exit Product, is Exit a buy etc.
"""

temp_dataframes = []

# Iterate the Split_df list
for i in range(len(split_dfs)):
#for i in range(1,2):

  split_dfs[i] = split_dfs[i].sort_values(by='time')
  # Convert 'time' column to datetime objects
  split_dfs[i]['time'] = pd.to_datetime(split_dfs[i]['time'])
  split_dfs[i]['avg_req_duration'] = (max(split_dfs[i]['time']) - min(split_dfs[i]['time'])) / len(split_dfs[i])
  split_dfs[i]['num_requests'] = len(split_dfs[i])
  split_dfs[i]['exit_product_id'] = split_dfs[i]['Product_Id'].tail(1).values[0]
  split_dfs[i]['exit_buy'] = split_dfs[i]['AddToCart'].tail(1).values[0]
  split_dfs[i]['entry_product_id'] = split_dfs[i]['Product_Id'].head(1).values[0]
  split_dfs[i]['entry_buy'] = split_dfs[i]['AddToCart'].head(1).values[0]

  #### Let's start with the Product Node, create a subset dataframe.
  # It will have Product_Id, Category, Department, url
  df_temp_product_nodes = split_dfs[i][['Product_Id', 'Category']]
  df_temp_product_nodes = df_temp_product_nodes.drop_duplicates()
  df_temp_product_nodes = df_temp_product_nodes.reset_index(drop=True)
  print(df_temp_product_nodes)

  # Filter df_product_features by Product_Id
  df_temp_product_features = df_product_features[df_product_features['Product_Id'].isin(df_temp_product_nodes['Product_Id'])]
  print(df_temp_product_features)

  # Create the user node and features
  df_temp_user_node = split_dfs[i][['ip_id']]
  df_temp_user_node = df_temp_user_node.drop_duplicates()
  df_temp_user_node = df_temp_user_node.reset_index(drop=True)
  print("!!!!")
  print(df_temp_user_node)


  avg_req_duration = split_dfs[i]['avg_req_duration'].mean()
  num_requests = split_dfs[i]['num_requests'].mean()
  exit_product_id = split_dfs[i]['exit_product_id'].mean()

  # Convert avg_req_duration to a numeric representation before concatenation
  avg_req_duration_seconds = avg_req_duration.total_seconds()

  # Create a DataFrame from the individual features
  df_temp_user_features = pd.DataFrame({
    'avg_req_duration': [avg_req_duration_seconds],
    'num_requests': [num_requests],
    'exit_product_id': [exit_product_id]
  })
  print(df_temp_user_features)


  # Edge / edge index
  temp_edge_index = split_dfs[i][["ip_id", "Product_Id"]].values.transpose()
  temp_edge_index = torch.tensor(temp_edge_index, dtype=torch.long)
  print(temp_edge_index)
  print(temp_edge_index.shape)


  ##### Build the dataset
  temp_data = HeteroData()

  # Set the number of nodes for 'user' and 'product'
  temp_data['user'].num_nodes = len(df_temp_user_features) # Assuming user_node_features is a list or array of user features
  temp_data['user'].id = df_temp_user_node['ip_id']
  temp_data['product'].num_nodes = len(df_temp_product_features) # Assuming df_product_features is a DataFrame or array of product features

  temp_data['user'].x = df_temp_user_features
  temp_data['product'].x = df_temp_product_features
  temp_data['user', 'view', 'product'].edge_index = temp_edge_index

  print(temp_data)
  print("~~~~")

"""Convert the HeteroData object into NetworkX object to visualize the graphs

###### Draw the graph
  graph = to_networkx(temp_data)  # Convert to undirected graph

  # Networkx seems to create extra nodes from our heterogeneous graph, so I remove them
  #isolated_nodes = [node for node in graph.nodes() if graph.out_degree(node) == 0]
  #[graph.remove_node(i_n) for i_n in isolated_nodes]

  print(graph.nodes(data=True))
  print(graph.edges(data=True))

  print(graph.number_of_edges())


  print("######")

  # Define colors for nodes and edges
  node_type_colors = {
      "user": "#4599C3",
      "product": "#ED8546",
  }

  node_colors = []
  labels = {}
  ###
  for node, attrs in graph.nodes(data=True):
    print(graph.out_degree(node))

    print(node)
    #print(attrs)
    #print(type(attrs))


    node_type = attrs.get("type")

    print(node_type)

  #  color = node_type_colors.get(node_type, 'gray') # Use get with default color
   # color = node_type_colors[node_type]
   # node_colors.append(color)
  #  if attrs.get("type") == "user":
   #     labels[node] = f"U{node}"
   # elif attrs.get("type") == "product":
    #    labels[node] = f"P{node}"


    # Instead of temp_data.node_type(node), check for node type based on node ID
    if node < temp_data['user'].num_nodes: # Assuming user nodes are numbered first
        node_type = "user"
    else:
        node_type = "product"

    color = node_type_colors.get(node_type, 'gray') # Use get with default color
    node_colors.append(color)
    if node_type == "user":
        labels[node] = f"U{node}"
    elif node_type == "product":
        labels[node] = f"P{node}"

  ###
  # Define colors for the edges
  edge_type_colors = {
      ("user", "buy", "product"): "#8B4D9E",
      ("user", "view", "product"): "#DFB825",
  }

  edge_colors = []
  for from_node, to_node, attrs in graph.edges(data=True):
      edge_type = attrs["type"]
      color = edge_type_colors[edge_type]

      graph.edges[from_node, to_node]["color"] = color
      edge_colors.append(color)


  # Draw the graph
  pos = nx.spring_layout(graph, k=2)
  nx.draw_networkx(
      graph,
      pos=pos,
      labels=labels,
      with_labels=True,
      node_color=node_colors,
      edge_color=edge_colors,
      node_size=600,
  )
  plt.show()
"""

print(len(split_dfs))
print(split_dfs[1])

"""Create custom GNN layers in Pytorch Geometric"""

import torch
import torch.nn as nn
from torch.nn import Linear
from torch_geometric.nn import MessagePassing
from torch_geometric.utils import add_self_loops, degree

class GCNConv(MessagePassing):
  def __init__(self, dim_in, dim_h):
    super().__init__(aggr='add')
    self.linear = Linear(dim_in, dim_h, bias=False)

  def forard(self, x, edge_index):
    edge_index, _ = add_self_loops(edge_index, num_nodes=x.size(0))

    # apply linear transformation
    x= self.linear(x)

    # compute the normalization factor
    row, col = edge_index
    deg = degree(col, x.size(0), dtype=x.dtype)
    deg_inv_sqrt = deg.pow(-0.5)
    deg_inv_sqrt[deg_inv_sqrt == float('inf')] = 0
    norm = deg_inv_sqrt[row] * deg_inv_sqrt[col]

    # start propogating
    out = self.propagate(edge_index, x=x, norm=norm)
    return out

  def message(self, x, norm):
    return norm.view(-1, 1) * x

# Initialize this object as a GCN layer
conv = GCNConv(16, 32)

"""Create a one-layer GAT model"""

print(data['user'].x)

import torch
import torch.nn.functional as F
from torch_geometric.nn import GAT
from torch_geometric.utils import to_undirected # Import to_undirected


model = GAT(in_channels=-1, hidden_channels=64, out_channels=4, num_layers=1)

optimizer = torch.optim.Adam(model.parameters(), lr=0.01, weight_decay=0.001)
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
data = data.to(device)
model = model.to(device)

# Convert node features to float and move to device
data['user'].x = torch.tensor(data['user'].x, dtype=torch.float).to(device)


# ----> Fix: Get the maximum node index across all node types <----
max_node_index = 0
for node_type in data.node_types:
    max_node_index = max(max_node_index, data[node_type].x.shape[0] -1 if data[node_type].x is not None else 0)

# Check the number of nodes in the user node type
num_user_nodes = data['user'].x.shape[0]

# Print for debugging
print(f"Maximum node index in edge_index: {max_node_index}")
print(f"Number of nodes in 'user': {num_user_nodes}")

# Assuming 'user' is your node type and ('user', 'buy', 'product') is your edge type
edge_index = data['user', 'buy', 'product'].edge_index

# -----> Fix: Clamp edges based on total number of nodes <-----
edge_index = edge_index.clamp(0, max_node_index)

# Convert to undirected if needed
edge_index = to_undirected(edge_index, num_nodes=max_node_index + 1) # Adjust if needed for your data format

# Update the edge_index in your data object
data['user', 'buy', 'product'].edge_index = edge_index

# test function
@torch.no_grad()
def test():
  model.eval()
  # Pass data['user'].x for node features
  pred = model(data['user'].x, data['user', 'buy', 'product'].edge_index).argmax(dim=1)
  correct = (pred[data['user'].test_mask] == data['user'].y[data['user'].test_mask]).sum()
  acc = int(correct) / int(data['user'].test_mask.sum())
  return float(acc)


# Create training loop
for epoch in range(100):
  model.train()
  optimizer.zero_grad()
  out = model(data['user'].x, data['user', 'buy', 'product'].edge_index)

  # ----> Fix: Apply mask correctly to user node predictions and labels <----

  # Ensure train_mask is 1D and has the correct number of user nodes
  train_mask = data['user'].train_mask.view(-1)
  train_mask = train_mask[:data['user'].x.shape[0]] # Limit to user nodes

  # ----> Fix: Get target labels using the train_mask <----
  target_labels = data['user'].y[train_mask]

  loss = F.cross_entropy(out[train_mask], target_labels)  # Use target_labels
  loss.backward()
  optimizer.step()
  print(f'Epoch: {epoch}, Loss: {loss.item()}, Accuracy: {test()}')

"""Heterogeneous implementation of GAT"""

from torch_geometric.nn import GATConv, Linear, to_hetero

class GAT(torch.nn.Module):
    def __init__(self, dim_h, dim_out, metadata):  # Add metadata to the constructor
        super().__init__()
        self.conv1 = GATConv((-1, -1), dim_h, add_self_loops=False)
        self.linear = Linear(dim_h, dim_out)
        self.metadata = metadata  # Store metadata

    def forward(self, x_dict, edge_index_dict):  # Modify forward to handle dictionaries
        # Iterate over node types and apply conv1 only to 'user'
        for node_type in self.metadata[0]:
            if node_type == 'user':
                x_dict[node_type] = self.conv1(x_dict[node_type], edge_index_dict[('user', 'buy', 'product')]).relu()

        # Apply linear layer to 'user' node embeddings
        x_dict['user'] = self.linear(x_dict['user'])
        return x_dict  # Return the updated x_dict


model = GAT(dim_h=64, dim_out=4, metadata=data.metadata()) # Pass metadata when creating the model
model = to_hetero(model, data.metadata(), aggr='sum')
optimizer = torch.optim.Adam(model.parameters(), lr=0.01)