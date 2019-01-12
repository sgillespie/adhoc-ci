# Adhoc CI
A set of continuous integration principals based on [Adhoc Continuous Integration](https://github.com/sgillespie/adhoc-continuous-integration). I hope these lead to the implementation of a CI software system some day.

The core values of Adhoc CI are

 * Open Source
 * Easy to Set Up
 * Easy to Scale
 * Usable on Bare Metal or Cloud
 * Builds are Configured in Source Code

## Introduction
We can think of the entire continuous integration (CI) process as a deployment pipeline. Each pipe in a pipeline represents a step in the build, test, and deploy lifecycle.

![Pipeline](doc/pipeline.png?raw=true "Pipeline")

Abstractly, a pipeline consists of:

 1. Input: the source code
 2. Pipes: Individual steps in the pipeline
 3. Gates: Filters between pipes
 4. Output: Whether the pipeline was successful
 
If any pipe in the pipeline fails, the next gate terminates the process and the whole pipeline is failed. Additionally, a gate can be either automatic or manual. Normally once a pipe is successfully completed, the next pipe is processed automatically. However, a manual pipe would require a human to initiate the continuation of the process.

## Pipeline Definition
Like many other CI tools, we use yaml to define the pipeline in the project source code. Here is a potential example:

    image: haskell:latest
    before_script: "cabal install --dependencies-only --enable-tests"
    after_script: "rm -r new-dist"
    
    pipeline:
      build: "cabal configure && cabal build"
      test: "cabal test"
      deploy: "./deploy-app.sh"
