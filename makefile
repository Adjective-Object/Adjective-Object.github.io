
all: html

html: site
	ls -l site
	./site rebuild 

watch: site
	./site rebuild
	./site watch

site: site.hs
	ghc --make -threaded -dynamic site.hs
