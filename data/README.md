# Data Storage Directory

This directory contains plain text storage for our dogfood Git server.

## Structure

- `repos/` - Repository metadata (one file per repo)
- `pushes/` - Raw push data for debugging (timestamped files)
- `objects/` - Git objects in plain text (sha.txt files)
- `refs/` - References in plain text (repo-name/ref-name.txt)

## Format Philosophy

Everything is plain text for easy debugging:
- Objects: SHA as filename, content as text
- Refs: One line per file with SHA
- Pushes: Raw protocol data for analysis

## Why Plain Text?

1. Easy to debug with `cat`, `grep`, `ls`
2. No dependencies (no SQLite, no JSON parser)
3. Fits the minimal server philosophy
4. Can actually see what we're storing
5. Makes lies obvious (empty files = "stored" objects)

This is intentionally naive storage - perfect for dogfooding!