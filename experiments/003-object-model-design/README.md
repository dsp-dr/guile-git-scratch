# Experiment 003: Git Object Model Design

## Goal
Design a functional object model for Git's core data structures in Guile3.

## Hypothesis
Git's object model (blob, tree, commit, tag) can be elegantly represented using Guile's record types and functional patterns.

## Implementation
See `object-model.org` for literate programming implementation.

## Results

### Object Hierarchy
- **git-object**: Base type with type, size, content
- **blob**: File contents
- **tree**: Directory structure with entries
- **commit**: Snapshot with metadata
- **tag**: Annotated reference

### Design Decisions
1. Use SRFI-9 records for immutable data structures
2. Separate logical model from serialization
3. Functional transformations between types
4. Bytevector-based content storage

## Integration Notes
This object model will be the foundation for all Git operations.

## Next Steps
- Experiment 004: SHA-1 hashing implementation
- Experiment 005: Zlib compression integration
- Integrate into main src/core/objects.scm