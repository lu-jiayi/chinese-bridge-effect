import pandas as pd
import json

# Read the Excel file
# Replace "path_to_excel.xlsx" with your actual file path
excel_data = pd.read_excel("C:/Users/aldos/Desktop/chinese-bridge-effect/exp3_stim.xlsx", sheet_name=0)

# Convert the DataFrame to JSON
json_data = excel_data.to_json(orient='records', indent=4)

# Write the JSON data to a file
with open("output3.json", "w", encoding='utf-8') as json_file:
    json.dump(json.loads(json_data), json_file, ensure_ascii=False, indent=4)

# Print confirmation
print("JSON file created successfully: output3.json")