# Contributing to Guile Git Implementation

Thanks for your interest in contributing! This project is an experimental implementation for learning purposes, focusing on functional programming patterns and FreeBSD integration.

## Development Guidelines

### Commit Conventions
Using conventional commits:
- `feat(module):` New features
- `fix(module):` Bug fixes  
- `exp(NNN):` Experiment work
- `docs:` Documentation updates
- `test:` Test additions/changes

### Testing Strategy
- Unit tests for all public APIs using SRFI-64
- Integration tests for command-line interface
- Property-based testing for critical algorithms
- Performance benchmarks for optimization targets
- **ROOF ROOF methodology** - Fun, dog-themed testing for engagement

### Code Style
- Prefer functional patterns over mutation
- Use pattern matching (ice-9 match) for clarity
- Document all public procedures
- Keep modules focused and cohesive
- **NO COMMENTS** unless explicitly requested

### Platform Considerations
- Primary target: FreeBSD with Guile 3.0+
- Document platform-specific issues (see `experiments/006-guile-freebsd-segfault/`)
- Use shell alternatives when Guile FFI has compatibility issues
- Test on multiple platforms when possible

## What We're Looking For

Contributions focusing on:
- **Functional programming patterns** in Scheme
- **Guile-specific optimizations** and idioms
- **FreeBSD integration** and platform features
- **Educational documentation** and literate programming
- **Creative AI integrations** and plugin development
- **Dog-themed humor** in commit messages and documentation 🐕

## Development Workflow

### 1. Experiment-Driven Development
Each major feature starts as a numbered experiment:
```bash
cd experiments
mkdir 007-your-feature-name
# Document your exploration in README.org
# Build and test incrementally
```

### 2. Dogfooding Process
We use our own Git server as soon as possible:
```bash
# Start dogfood server
./start-dogfood-server.sh

# Add remote and test
git remote add dogfood git://localhost:9418/your-repo.git
git push dogfood main

# Check what was stored
ls data/pushes/
./plugins/woof-ai-summary.sh latest
```

### 3. Testing Protocol
```bash
# Run tests
gmake test

# ROOF ROOF testing
gmake roof-roof-test

# Check status
gmake status
```

## Getting Started

### Prerequisites
```bash
# FreeBSD packages (required)
pkg install guile3 gmake tmux curl

# AI integration (optional)
pkg install ollama

# Development tools (optional)  
pkg install emacs gdb
```

### Development Setup
```bash
git clone https://github.com/dsp-dr/guile-git-scratch.git
cd guile-git-scratch

# Check environment
cd experiments/000-deps-check && gmake run

# Start dogfooding
./start-dogfood-server.sh

# Begin development!
```

## Known Issues

### Server Infrastructure
- Port binding conflicts between server versions
- Socket operations sometimes hang/block
- Process management needs improvement
- See `TESTING-LOG.md` for detailed issue documentation

### FreeBSD Compatibility
- Guile3 segfaults with `posix_spawn_file_actions_addclosefrom_np()`
- Shell-based workarounds implemented
- See `experiments/006-guile-freebsd-segfault/` for details

## Project Structure

```
experiments/          # Numbered experiments (000-999)
src/core/            # Core Git functionality
src/storage/         # Storage systems
plugins/             # AI and other plugins
tests/               # Test suite
data/                # Runtime data (gitignored)
```

## Communication

- **Issues:** Document problems thoroughly with experiments
- **Features:** Start with experiment-driven exploration
- **Questions:** Check existing experiments and documentation first

## Code of Conduct

- Be kind and constructive
- Document your discoveries
- Embrace the messy development process
- Have fun with dog-themed commits! 🐕
- Remember: this is for learning, not production

---

*"The best contributions are the ones that teach us something new. WOOF WOOF!"* 🐕