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
                    "commit": commit.hexsha,
                    "timestamp": commit.committed_datetime.isoformat(),
                    "prices": prices
                })
            except json.JSONDecodeError:
                continue
    return data

def load_existing_data():
    if not os.path.exists("gas_prices_history.json"):
        return []
    with open("gas_prices_history.json", "r", encoding="utf-8") as f:
        return json.load(f)

def get_missing_commits(repo, file_path, existing_data):
    existing_commits = {
        item.get("commit")
        for item in existing_data
        if item.get("commit")
    }
    existing_timestamps = {
        item.get("timestamp")
        for item in existing_data
        if item.get("timestamp")
    }

    all_commits = list(repo.iter_commits(paths=file_path))
    missing_commits = [
        commit
        for commit in all_commits
        if commit.hexsha not in existing_commits
        and commit.committed_datetime.isoformat() not in existing_timestamps
    ]
    return missing_commits, all_commits

def write_last_commit(commits):
    if not commits:
        return
    with open("last_commit.txt", "w") as f:
        f.write(commits[0].hexsha)

def main():
    print("Cloning repository...")
    repo, tmp_dir = clone_repo()

    full_data = load_existing_data()

    print("Getting missing commits...")
    commits, all_commits = get_missing_commits(repo, FILE_PATH, full_data)

    if not commits:
        write_last_commit(all_commits)
        print("Engin ný eða vöntuð commit — ekkert að uppfæra.")
        return

    print(f"Collecting gas price data from {len(commits)} missing commits...")
    new_data = collect_price_data(repo, commits)

    # Sameina og vista
    full_data.extend(new_data)
    with open("gas_prices_history.json", "w", encoding="utf-8") as f:
        json.dump(full_data, f, indent=2, ensure_ascii=False)

    # Vista nýjasta SHA
    write_last_commit(all_commits)

    print(f"Skráð {len(new_data)} nýjar/vantaðar athuganir í gas_prices_history.json")

if __name__ == "__main__":
    main()
