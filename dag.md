# Compiler DAG from first principles

The current compiler DAG is based off code that existed in rebar 2.x. Over the years, we've reworked the interfaces surrounding it in major ways that improved things, but the core DAG itself is still lacking.

## What it Currently Does

- Ensure the ebin/ dir exists and is in path
    - may be required for erl_first_files to be visible
- Ensure the compile module is loaded for tests in functionality across versions
- Called per app; Two runs, one for the src_dir, one for the extra_src_dir
- get the context (src/include/extra/mappings) from the compiler module
- get the opts, assemble full length include dirs
- find all the source files in scope by recursive descent and regexes
- Set up the DAG
    - updates the DAG for new/missing files first
    - updates the DAG for all the changed files
    - does some form of recursive descent based on all the files this one relied on
    - The compiler module gives lists of dependencies
- ask the compiler module which of the source files are needed
    - specified by those that must be done first and then the rest
- compile the first files
    - compile the rest of them in parallel order when possible

## Analysis

## What needs to be done

- Detect change based on content rather than timestamps: q: speed?

- Good demarcation between build-time and run-time dependencies, when they need to be loaded or to trigger rebuilds
    - include files are build time
    - include files are transitive and can be nested
    - parse transforms are build time
    - behaviours are build time
        - can be cross-project
        - does a change in behaviour imply a change in the calling mod though?
          probably not, but they imply a build-time dependency in a compiler check
    - function calls are run-time
    - current build order re: include files are based on runtime settings
    - know when some files are conditionally included or not? Relies on epp
        - not knowing when files could be resolved can be a real problem because
          it forces a "if not found, ignore" policy. Maybe we can use epp explicitly
          instead of erl_syntax
    - imported functions are not compile-time, it's a syntax trick
    - are overwritten transitive deps still available?

- write tests:
    - app files without specifying other top-level apps
    - include files changing
    - include_lib files changing
    - behaviours being visible
    - behaviour definition changing and being picked up (may not need re-compiling, but mandates visibility)
 
- finding files should not be recursive for extra_src_dirs
- need consistency in what paths and include paths are tracked and apply it uniformly

- knowing the compiler version used to know when to re-compile when
  hitting a dep built with a newer version

https://apenwarr.ca/log/20181113 for smart mtime usage
    !- use that to calculate hashing needs (for docker shit that will change them regardless)
