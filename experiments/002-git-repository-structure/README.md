# Experiment 002: Git Repository Structure Exploration

## Goal
Understand the structure of a Git repository (.git directory) to inform our Guile implementation.

## Hypothesis
Git stores all data in a well-defined directory structure that we can replicate functionally in Guile.

## Implementation
Explore an actual .git directory to understand its layout.

## Results

### Directory Structure
```
.git/
├── config          # Repository configuration
├── description     # Repository description
├── HEAD           # Current branch reference
├── hooks/         # Hook scripts
├── info/          # Additional info
│   └── exclude    # Local ignore patterns
├── objects/       # Object database
│   ├── info/
│   └── pack/      # Packfiles
├── refs/          # References
│   ├── heads/     # Branches
│   └── tags/      # Tags
└── index          # Staging area
```

### Key Observations
1. Objects are stored in `objects/` with 2-char subdirectories
2. References are plain text files containing SHA-1 hashes
3. The index is a binary file requiring careful parsing
4. Configuration uses INI-style format

## Integration Notes
Our Guile implementation will need modules for:
- Object storage and retrieval
- Reference management
- Index file handling
- Config file parsing

## Next Steps
- Experiment 003: Design the object model
- Experiment 004: Implement SHA-1 and zlib