import pandas as pd
import ipinfo

access_token = '3aac4fa63b3849' # Optional,But added because we had more data
handler = ipinfo.getHandler(access_token)
count = 0  

def get_location(ip):
    global count 
    try:
        count += 1
        details = handler.getDetails(ip)
        
        city = details.city if details.city else None
        country = details.country_name if details.country_name else None
        state = details.region if details.region else None
        
        print(f"Processing IP {count}: {ip} -> City: {city}, Country: {country}, State: {state}")
        return city, country, state
    except Exception as e:
        print(f"Exception for IP {ip}: {e}")
        return None, None, None

csv_file = 'tokenized_access_logs.csv'  
df = pd.read_csv(csv_file)

df['City'], df['Country'], df['State'] = zip(*df['ip'].apply(get_location))

output_file = 'tokenized_access_logs_with_location_2.csv'
df.to_csv(output_file, index=False)

print(f"Updated CSV saved as {output_file}")



# Available Info
    # {
    # "ip": "24.246.105.2",
    # "hostname": "18f66002.superior-technology.net",
    # "city": "New York City",
    # "region": "New York",
    # "country": "US",
    # "loc": "40.7143,-74.0060",
    # "org": "AS53720 Superior Technology Solutions LLC",
    # "postal": "10001",
    # "timezone": "America/New_York"
    # }
