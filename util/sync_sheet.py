import os
import json
import gspread
from oauth2client.service_account import ServiceAccountCredentials

# Define the scope
scope = ["https://spreadsheets.google.com/feeds", "https://www.googleapis.com/auth/drive"]

# Load credentials from environment variable
creds_dict = json.loads(os.environ['GOOGLE_SERVICE_ACCOUNT_JSON'])
creds = ServiceAccountCredentials.from_json_keyfile_dict(creds_dict, scope)
client = gspread.authorize(creds)

# Open the sheet and get all records
# https://docs.google.com/spreadsheets/d/1xIIwU8mYHAL4q0G65Xant5J8eVuRhV3KZJBXdOzXoiY/edit?usp=drivesdk
sheet = client.open_by_key("1xIIwU8mYHAL4q0G65Xant5J8eVuRhV3KZJBXdOzXoiY").worksheet("Data for Matt")
data = sheet.get("A1:K39")
headers = data[0]
data = [dict(zip(headers, row)) for row in data[1:]]

# Save as JSON
with open('data.json', 'w') as f:
    json.dump(data, f, indent=4)
