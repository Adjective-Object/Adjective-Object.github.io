
stsite=stack exec site

all: html

html: site.hs
	stack build
	$(stsite) rebuild 

watch: site.hs
	stack build
	$(stsite) watch

clean:
	$(stsite) clean
	stack build

