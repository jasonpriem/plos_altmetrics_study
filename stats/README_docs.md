#Documentation for stats results

Documentation for stats results is generated automatically using [dexy](http://blog.dexy.it/).

To generate documentation, first run this to set up necessary subdirectories:

    dexy --setup docs_src

And thereafter run:

    dexy -s docs_src

To see it working, you can check out

	tail -f logs/dexy.log
		
The resulting docs will be in the new cache subdirectory, and images will be in the new artifacts subdirectory.  The final pages are then added and committed to the [project wiki repository](https://github.com/jasonpriem/plos_altmetrics_study/wiki) and the images are moved to an image server (with the final doc pages image root path modified accordingly).



