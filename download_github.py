import github
from github import Github
from github import RateLimitExceededException
from base64 import b64decode
from datetime import datetime
from os.path import dirname
from os import makedirs
import time
from github import Github
from base64 import b64decode
from tqdm import tqdm
import calendar
import json

token = "" 
g = Github(token)
g.per_page = 30

def tryfetch(fun):
    while(True):
        try:
            return fun() 
        except StopIteration:
            break
        except RateLimitExceededException as err:
            print(err)
            time_remaining = (g.get_rate_limit().search.reset - datetime.utcnow()).total_seconds()
            print(f"Sleeping for {time_remaining}")
            time.sleep(61)
            print("Resuming")

github.PaginatedList.PaginatedList._oldFetch = github.PaginatedList.PaginatedList._fetchNextPage
github.PaginatedList.PaginatedList._fetchNextPage = lambda self: tryfetch(lambda : self._oldFetch())


count = 0
with open("log.jsonl", "w+") as l:
    while count < 50000:
        search = tryfetch(lambda: g.search_code(query='language:Cuda'))
        for item in tqdm(search,total=tryfetch(lambda: search.totalCount)):
            count += 1
            l.write(json.dumps({
                "repo_name": item.repository.full_name,
                "repo_url": item.repository.html_url,
                "file_url": item.html_url,
                "file_path": item.path
                }))
            l.write("\n")
            l.flush()
            print(item)
            # rl = g.get_rate_limit().search
            file_data = b64decode(item.content)
            makedirs(dirname(item.repository.full_name+"/"+item.path), exist_ok=True)
            with open(item.repository.full_name+"/"+item.path, "wb") as f:
                f.write(file_data)

