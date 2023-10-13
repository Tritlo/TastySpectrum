# Known Issues / Troubleshooting 

This file should document some of the 

## module mismatch with .tix/.mix file hash number

Error: 

```
Running 1 test suites...
Test suite test-project: RUNNING...
in module 'Main'
Hpc failure: module mismatch with .tix/.mix file hash number
(perhaps remove test-project.exe.tix file?)

Test suite test-project: FAIL
```

Likely there are some (outdated) coverage artifacts. 

Make sure to delete all mixfiles by running `find . -name "*.tix" -delete`. 

## Missing .mix File (Windows Paths)

Error: 

```shell
cabal test --test-options "--get-spectrum"
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
...
Building test suite 'test-pandoc' for your-project ...
Running 1 test suites...
Test suite test-pandoc: RUNNING...
test-pandoc.exe: can not find Main in .hpc
CallStack (from HasCallStack):
  error, called at libraries\\hpc\\Trace\\Hpc\\Mix.hs:122:15 in hpc-0.6.1.0:Trace.Hpc.Mix
```

This is likely due to a windows machine. We currently only support linux. For Windows, consider WSL. 

## can not find Main in .hpc

Error: 
```
...
Running 1 test suites...
Test suite test-project: RUNNING...
test-project: can not find Main in .hpc
CallStack (from HasCallStack):
  error, called at libraries/hpc/Trace/Hpc/Mix.hs:122:15 in hpc-0.6.1.0:Trace.Hpc.Mix

Test suite test-project: FAIL
...
```

Please check the following: 
Do you have an .hpc directory with (sufficient) files? Are the files correct?

If yes, make sure you run with the correct flags. You might be missing the `--hpc-dir` option or are pointing to a wrong location.

If not, check if your projects `.cabal` files contain ghc-options for `-fhpc` and consider a re-compilation of the project. Before recompiling, delete the `.hpc` folder and all existing `.tix`-files with a command such as `find . -name "*.tix" -delete`. 