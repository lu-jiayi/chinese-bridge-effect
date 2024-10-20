import pandas as pd
import json

# Read the Excel file
# Replace "path_to_excel.xlsx" with your actual file path
excel_data = pd.read_excel("C:/Users/aldos/Desktop/chinese-bridge-effect/bridge_stimuli.xlsx", sheet_name=0)

# Convert the DataFrame to JSON
json_data = excel_data.to_json(orient='records', indent=4)

# Write the JSON data to a file
with open("output.json", "w", encoding='utf-8') as json_file:
    json_file.write(json_data)

# Print confirmation
print("JSON file created successfully: output.json")