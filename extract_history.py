#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 23 09:10:08 2025

@author: agustarnorsson
"""

import os
import json
import tempfile
from git import Repo
from tqdm import tqdm
# from datetime import datetime

# Constants
REPO_URL = "https://github.com/gasvaktin/gasvaktin.git"
FILE_PATH = "vaktin/gas.json"

def clone_repo():
    tmp_dir = tempfile.mkdtemp()
    repo = Repo.clone_from(REPO_URL, tmp_dir)
    return repo, tmp_dir

def extract_file_at_commit(repo, commit, file_path):
    try:
        blob = commit.tree / file_path
        return blob.data_stream.read().decode("utf-8")
    except Exception:
        return None

def collect_price_data(repo, commits):
    data = []
    for commit in tqdm(commits, desc="Extracting gas prices from commits"):
        content = extract_file_at_commit(repo, commit, FILE_PATH)
        if content:
            try:
                prices = json.loads(content)
                data.append({
                    "timestamp": commit.committed_datetime.isoformat(),
                    "prices": prices
                })
            except json.JSONDecodeError:
                continue
    return data

def get_new_commits(repo, file_path, last_sha=None):
    all_commits = list(repo.iter_commits(paths=file_path))
    if last_sha:
        for i, commit in enumerate(all_commits):
            if commit.hexsha.startswith(last_sha):
                return all_commits[:i]  # bara ný commit
    return all_commits

def main():
    print("Cloning repository...")
    repo, tmp_dir = clone_repo()

    # Sækja síðasta SHA ef hann er til
    last_sha = None
    if os.path.exists("last_commit.txt"):
        with open("last_commit.txt", "r") as f:
            last_sha = f.read().strip()

    print("Getting new commits...")
    commits = get_new_commits(repo, FILE_PATH, last_sha)

    if not commits:
        print("Engin ný commit — ekkert að uppfæra.")
        return

    print(f"Collecting gas price data from {len(commits)} new commits...")
    new_data = collect_price_data(repo, commits)

    # Hlaða eldri gögnum ef þau eru til
    full_data = []
    if os.path.exists("gas_prices_history.json"):
        with open("gas_prices_history.json", "r", encoding="utf-8") as f:
            full_data = json.load(f)

    # Sameina og vista
    full_data.extend(new_data)
    with open("gas_prices_history.json", "w", encoding="utf-8") as f:
        json.dump(full_data, f, indent=2, ensure_ascii=False)

    # Vista nýjasta SHA
    with open("last_commit.txt", "w") as f:
        f.write(commits[0].hexsha)

    print(f"Skráð {len(new_data)} nýjar athuganir í gas_prices_history.json")

if __name__ == "__main__":
    main()
