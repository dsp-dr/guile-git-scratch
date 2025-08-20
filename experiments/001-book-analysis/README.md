# Experiment 001: WYAG Book Structure Analysis

## Goal
Extract and understand the key concepts from "Write Yourself a Git!" to plan the Guile3 implementation.

## Hypothesis
The book follows a progressive approach from basic objects to full Git functionality that can be mapped to functional Guile patterns.

## Implementation
Parse the WYAG HTML to extract chapter structure and key commands.

## Results

### Core Commands to Implement
1. **init** - Create repositories
2. **hash-object** - Store objects
3. **cat-file** - Read objects
4. **log** - Show commit history
5. **ls-tree** - Show tree contents
6. **checkout** - Switch branches/restore files
7. **show-ref** - List references
8. **tag** - Create tags
9. **rev-parse** - Parse revision names
10. **ls-files** - Show index
11. **check-ignore** - Check gitignore rules
12. **status** - Show working tree status
13. **rm** - Remove from index
14. **add** - Add to index
15. **commit** - Create commits

### Object Types
- **Blob**: File contents
- **Tree**: Directory listings
- **Commit**: Snapshots with metadata
- **Tag**: Annotated tags

### Key Concepts
- Content-addressable storage with SHA-1
- Zlib compression for objects
- Index file for staging area
- References (branches, tags)
- Configuration files

## Integration Notes
Each command will become a Guile module with functional interfaces.

## Next Steps
- Experiment 002: Explore actual .git directory structure
- Experiment 003: Design object model in Guile