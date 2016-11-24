#!/usr/bin/env python
import requests
import json

if __name__ == '__main__':
    mp_endpoint = 'http://127.0.0.1:5984/myblog/_design/sofa/_micropub'
    data = { 'h': 'entry',
             'content': 'Micropub test of creating a basic h-entry',
             'category': [ 'foo', 'bar' ]
    }
    content_type = 'application/x-www-form-urlencoded; charset=utf-8'
    json_data = {
        'type': ['h-entry'],
        'properties': {
            "content": ["hello world"],
            "name": ["My Micropub Post"],
            "category": ["foo","bar"],
            "photo": ["https://photos.example.com/592829482876343254.jpg"]   
        }
    }
    
    data = json.dumps(json_data)
    content_type = 'application/json'
    r = requests.post(mp_endpoint, headers={'Authorization': 'Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJtZSI6Imh0dHBzOlwvXC9oYXp5Ymx1ZS5tZVwvIiwiaXNzdWVkX2J5IjoiaHR0cHM6XC9cL3Rva2Vucy5pbmRpZWF1dGguY29tXC90b2tlbiIsImNsaWVudF9pZCI6Imh0dHBzOlwvXC9taWNyb3B1Yi5yb2Nrc1wvIiwiaXNzdWVkX2F0IjoxNDc5MzEyODQwLCJzY29wZSI6ImNyZWF0ZSB1cGRhdGUgZGVsZXRlIHVuZGVsZXRlIiwibm9uY2UiOjQ4MDI4OTgwNn0.x1664uxTrGxbkjnPR6b5ANlxOaCsx5szkPI94ke7ZAs',
                                            'Content-Type': content_type },
                      data=data)
    print(r.status_code, r.text, r.headers)
