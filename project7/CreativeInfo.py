import csv
import requests

ACCESS_TOKEN = ""
ACCOUNT_ID = ""

def getWriteCreativeInfo(creative):
    # Getting the selected creative info
    creative_id = creative['id']
    api_url = "https://graph.intern.facebook.com/v3.3/{}?fields=thumbnail_url&access_token={}".format(creative_id, ACCESS_TOKEN)
    r = requests.get(url=api_url)

    # Open CSV file and write row
    creative_info_data = open('creativeInfoData.csv', 'aw')
    csvwriter = csv.writer(creative_info_data)
    csvwriter.writerow(r.json().values())
    creative_info_data.close()

# Getting the list of creatives
api_url = "https://graph.intern.facebook.com/v3.3/act_{}/adcreatives?access_token={}".format(ACCOUNT_ID, ACCESS_TOKEN)
r = requests.get(url=api_url)
creative_list = r.json()['data']

# Map the creative to get the creative Info
map(getWriteCreativeInfo, creative_list)
