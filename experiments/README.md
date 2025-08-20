# Experiments Overview

This directory contains experiment-driven development for implementing a Git client and server in Guile3, based on "Write Yourself a Git!" (WYAG).

## Experiment Index

- **000-009**: Environment setup and book analysis
  - `000-deps-check`: Verify Guile3, FreeBSD, toolchain
  - `001-book-analysis`: Extract key concepts from WYAG
  - `002-git-repository-structure`: Explore .git directory structure
  - `003-object-model-design`: Design Git objects in Guile (blob, tree, commit, tag)
  - `004-sha1-and-zlib`: Implement SHA-1 hashing and zlib compression
  - `005-config-parser`: Git config file parsing
  
- **010-029**: Core Git operations
  - `010-init-command`: Repository initialization
  - `011-hash-object`: Object creation and storage
  - `012-cat-file`: Object reading and display
  - `013-log-command`: Commit history traversal
  - `014-tree-parsing`: Tree object manipulation
  - `015-checkout-command`: Working tree operations
  
- **030-049**: References and branching
  - `030-refs-implementation`: Reference storage and manipulation
  - `031-tags-lightweight`: Lightweight tags
  - `032-tag-objects`: Annotated tag objects
  - `033-branches`: Branch management
  - `034-rev-parse`: Object name resolution
  
- **040-059**: Index and staging
  - `040-index-format`: Index file structure
  - `041-index-parsing`: Reading the index
  - `042-status-command`: Working tree status
  - `043-add-command`: Staging changes
  - `044-rm-command`: Removing from index
  - `045-commit-command`: Creating commits
  
- **050-069**: Advanced features
  - `050-packfiles`: Packfile format exploration
  - `051-network-protocol`: Git wire protocol
  - `052-fetch-command`: Remote fetching
  - `053-push-command`: Remote pushing
  - `054-merge-algorithms`: Three-way merge
  
- **070-089**: Server implementation
  - `070-server-protocol`: Git server protocol
  - `071-receive-pack`: Receiving pushes
  - `072-upload-pack`: Serving fetches
  - `073-hooks-system`: Server-side hooks
  
- **090-099**: Release preparation
  - `090-api-stability`: API review
  - `095-performance-benchmarks`: Performance testing
  - `099-integration-tests`: Full system validation

## Running Experiments

```bash
# List all experiments
gmake list

# Run a specific experiment
gmake run EXP=001-book-analysis

# Test all experiments
gmake test

# Create a new experiment
gmake new
```