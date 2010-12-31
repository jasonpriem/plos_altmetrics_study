#Documentation for stats results

Documentation for stats results is generated automatically using [dexy](http://blog.dexy.it/).

To generate documentation, first run this to set up necessary subdirectories:

    dexy --setup docs_src

And thereafter run:

    dexy -s docs_src

The resulting docs will be in the new cache subdirectory, and images will be in the new artifacts subdirectory.  To see it working, you can check out

    tail -f logs/dexy.log



