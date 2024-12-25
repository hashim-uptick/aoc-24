The following is used to run the "performant" file with the best possible performance:

```
raco exe solution-demod.rkt  # creates a file called solution-demod
raco dist some-folder solution-demod  # creates a distributable file solution-demod in some-folder
cd some-folder/bin
./solution-demod
```

