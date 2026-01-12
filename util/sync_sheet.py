import os
import json
import gspread
from oauth2client.service_account import ServiceAccountCredentials
import re

def fetch_sheet(sheet_key, worksheet_title):
    # Load raw data from gspread (assumed to be data.json from previous step)
    # Define the scope
    scope = ["https://spreadsheets.google.com/feeds", "https://www.googleapis.com/auth/drive"]

    # Load credentials from environment variable
    creds_dict = json.loads(os.environ['GOOGLE_SERVICE_ACCOUNT_JSON'])
    creds = ServiceAccountCredentials.from_json_keyfile_dict(creds_dict, scope)
    client = gspread.authorize(creds)

    # Open the sheet and get all records
    # https://docs.google.com/spreadsheets/d/1xIIwU8mYHAL4q0G65Xant5J8eVuRhV3KZJBXdOzXoiY/edit?usp=drivesdk
    sheet = client.open_by_key(sheet_key).worksheet(worksheet_title)
    data = sheet.get("A1:K39")
    headers = data[0]
    return [dict(zip(headers, row)) for row in data[1:]]


def parse_prereqs(text):
    text = text.strip()
    if text.lower() in ["n", "", "none", "unknown", "consent"]:
        return {"tag": "None"}
    
    # Check for credit count: "90+ cr" -> {"tag": "CreditCount", "contents": 90}
    credit_match = re.search(r'(\d+)\+?\s*cr', text, re.IGNORECASE)
    if credit_match:
        return {"tag": "CreditCount", "contents": int(credit_match.group(1))}
    
    # Handle AND logic (split by semicolon)
    if ";" in text:
        parts = text.split(";")
        return {
            "tag": "And",
            "contents": [parse_prereqs(p) for p in parts if p.strip()]
        }
    
    # Handle OR logic (split by " or ")
    if " or " in text.lower():
        parts = re.split(r'\s+or\s+', text, flags=re.IGNORECASE)
        return {
            "tag": "Or",
            "contents": [parse_prereqs(p) for p in parts if p.strip()]
        }
    
    # Single Course Code (Sanitize to MATH134)
    code = text.replace(" ", "").upper()
    return {"tag": "CourseCode", "contents": code}

def parse_availability(row):
    seasons = []
    fw = row.get("Fall/Winter", "").lower()
    ss = row.get("Spr/Sum", "").lower()
    
    # Mapping logic for different seasons
    if any(x in fw for x in ["both", "fall"]): seasons.append("Fall")
    if any(x in fw for x in ["both", "winter"]): seasons.append("Winter")
    if any(x in ss for x in ["both", "spring"]): seasons.append("Spring")
    if any(x in ss for x in ["both", "summer"]): seasons.append("Summer")
    return list(dict.fromkeys(seasons))

def transform():

    raw_data = fetch_sheet("1xIIwU8mYHAL4q0G65Xant5J8eVuRhV3KZJBXdOzXoiY", "Data for Matt")
    processed = []
    for row in raw_data:
        # Skip empty rows or rows without a course name
        if not row or "Course" not in row or not row["Course"]:
            continue
            
        # Clean Course Code (Removes " OR" suffix found in some of your cells)
        code = row["Course"].strip().upper()
        code = re.sub(r'\s+OR$', '', code) # Clean "BOT 340 OR"
        code = code.replace(" ", "")

        # Map fields to your Haskell Schema
        item = {
            "code": code,
            "credits": 3,  # Defaulting to 3 as it's missing from the sheet
            "prereqs": parse_prereqs(row.get("Pre-Reqs", "n")),
            "availability": parse_availability(row),
            "requiresLab": None
        }

        # Map Co-Reqs to requiresLab if a code exists
        coreq_text = row.get("Co-Reqs", "n")
        if coreq_text.lower() != "n":
            coreq_match = re.search(r'([A-Z]{2,}\s*\d+)', coreq_text)
            if coreq_match:
                item["requiresLab"] = coreq_match.group(1).replace(" ", "")

        processed.append(item)

    with open('catalog.json', 'w') as f:
        json.dump(processed, f, indent=4)

if __name__ == "__main__":
    transform()
