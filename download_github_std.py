import urllib
from urllib.request import urlopen
from urllib.parse import urlencode, quote, urlparse, parse_qs
import json
import time

"""
curl -G https://api.github.com/search/code       \
    --data-urlencode "q=language:Cuda" \
    --data-urlencode "type=code"                          \
    --data-urlencode "l=Cuda"                          \
    -H "Accept: application/vnd.github.v3+json"  -H "Authorization: token ****************************************" -I > headers
"""


def read_pages(response):
    return int(parse_qs(urlparse(response.headers.get("link").split(", ")[-1].split("; ")[0].strip("<>")).query)["page"][0])

def read_request_limit(response):
    return int(response.headers.get("X-RateLimit-Remaining"))

def extract_data(data):
    out = []
    for item in data["items"]:
        out.append({
            "file_url": item["html_url"],
            "repository_name": item["repository"]["full_name"],
            "repository_url": item["repository"]["html_url"]
            })
    return out

def make_request(token, page=1):
    data = {
        "q": "language:Cuda",
        "type": "code",
        "l": "Cuda",
        "per_page": 100,
        "page": page
    }
    request = urllib.request.Request(f"https://api.github.com/search/code?{urlencode(data)}")
    request.add_header("Accept", "application/vnd.github.v3+json")
    request.add_header("Authorization", f"token {token}")
    return request

token = ""

initial_page = urlopen(make_request(token))

with open('kernels.jsonl', 'w') as f:
    body = json.loads(initial_page.read())
    process = extract_data(body)
    for item in process:
        f.write(json.dumps(item))
        f.write("\n")
        f.flush()
    for page in range(2,read_pages(initial_page)+1):
        time.sleep(120)
        next_page = urlopen(make_request(token))
        body = json.loads(next_page.read())
        process = extract_data(body)
        for item in process:
            f.write(json.dumps(item))
            f.write("\n")
            f.flush()

