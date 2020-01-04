# Adhoc CI
Adhoc CI is a set of continuous integration principals intended to address problems with
existing CI software tools.

According to these principals, I believe a well-crafted CI tool should

 1. Be Open Source
 2. Be Easy to Setup
 3. Work on Bare Metal or Cloud
 4. Have Decentralized Build Configuration in Source Code

## Concepts
We can think of the entire continuous integration (CI) process as a deployment
pipeline. Each pipe in a pipeline represents an individual step in the build in the build,
test and deployment lifecycle. In Adhoc CI, we refer to pipes as stages.

Abstractly, a pipeline consists of:

 1. Input: the source code
 2. Stages: Steps in the pipeline
 3. Jobs: Individual steps in a stage
 3. Gates: Filters between pipes
 4. Output: Whether the pipeline was successful

A stage represents steps in a pipeline. For example, a typical pipeline might have stages
Build, Test and Deploy. A stage can be further divided into jobs.
 
If any stage in the pipeline fails, the next gate terminates the process and the whole
pipeline is failed. In addition, jobs can have conditions attached. For example, a
specific step can be skipped based on the branch name or whether it's building a
tag. Gates can also be manual; In other words, it requires a human initiate the continuous
of the process.

## Pipeline Definition
A pipeline is configured in a file `adhoc-ci.yml` in the root of the project source
code. For example:

    pipeline:
      - name: build
        commands:
          - cabal configure
          - cabal build
          - cabal test
      - name: deploy to qc
        commands:
          - ./deploy-to-qc.sh
      - name: deploy to production
        commands:
          - ./deploy-to-production.sh

## Building
In order to build this project you will need

 * GHC
 * Cabal (>= 2.4)

Use the standard cabal commands to build the project:

    cabal new-configure --enable-tests
    cabal new-build
    cabal new-test --test-show-details=direct

To install it, use new-install

    cabal new-install

Alternatively, there is a script that uses `watchman-make` to watch for changes and
rebuild

    ./build-watch.sh

## Running
In order to run the pipeline, run the executable `adhoc-ci`

    adhoc-ci <STAGE1> <STAGE2>

where `<STAGE1>` and `<STAGE2>` are stages you want to run.