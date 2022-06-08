import requests
import json
import pandas as pd
import time


def get_hikes(state):
    print(f'scraping {state} ...')

    headers = {
        'Accept': '*/*',
        'Accept-Language': 'en-US,en;q=0.9',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
        'DNT': '1',
        'Origin': 'https://www.alltrails.com',
        'Pragma': 'no-cache',
        'Referer': 'https://www.alltrails.com/',
        'Sec-Fetch-Dest': 'empty',
        'Sec-Fetch-Mode': 'cors',
        'Sec-Fetch-Site': 'cross-site',
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.63 Safari/537.36',
        'content-type': 'application/x-www-form-urlencoded',
        'sec-ch-ua': '" Not A;Brand";v="99", "Chromium";v="102", "Google Chrome";v="102"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'x-algolia-api-key': f'{API_KEY}',
        'x-algolia-application-id': f'{APP_ID}',
    }

    data = '{"query":"","hitsPerPage":1000,"attributesToRetrieve":["description","ID","_geoloc","activities","area_id","area_name","area_slug","avg_rating","city_id","city_name","country_id","country_name","created_at","difficulty_rating","duration_minutes","duration_minutes_cycling","duration_minutes_hiking","duration_minutes_mountain_biking","duration_minutes_trail_running","elevation_gain","filters","has_profile_photo","is_closed","is_private_property","length","name","num_photos","num_reviews","photo_count","popularity","profile_photo_data","route_type","slug","state_id","state_name","type","units","user","verified_map_id","visitor_usage"],"filters":"(state_id=' + str(state_codes[
        state]) + ') AND ((length>=0)) AND ((elevation_gain>=0)) AND type:trail","attributesToHighlight":[],"responseFields":["hits","hitsPerPage","nbHits"]}'

    response = requests.post(
        'https://9ioacg5nhe-dsn.algolia.net/1/indexes/alltrails_index3/query?x-algolia-agent=Algolia%20for%20JavaScript%20(4.8.6)%3B%20Browser', headers=headers, data=data)
    json_data = json.loads(response.text)

    with open(f'trails/{state}.json', 'w') as f:
        json.dump(json_data, f)

    time.sleep(5)


state_codes = {'ma': 22, 'nh': 30, 'wa': 46,
               'me': 20, 'pa': 39, 'nj': 31, 'ny': 33}
credentials = pd.read_csv('backend/credentials.csv').squeeze()
API_KEY = credentials['key']
APP_ID = credentials['id']

[get_hikes(state) for state in state_codes.keys()]
